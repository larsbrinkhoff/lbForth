\ -*- forth -*- Copyright 2017 Pip Cet

\ Nucleus for asm.js.

include targets/asmjs/next.fth

start-code
"use strict";

var params = {
    memsize: 1024 * 1024,
    fsoff: 768 * 1024,
    dictoff: 16 * 1024,
    sp0: 763 * 1024,
    rp0: 767 * 1024,
};

var read_file_async;
var read_line;
var bye;
var gInputLines = [];
var resume_string = undefined;
var put_string;

function forth_input(str)
{
    str = str.replace(/\n$/, "");
    gInputLines.push(str);
    resume("//line");
}

if (typeof(os) !== "undefined") {
    /* SpiderMonkey shell */

    read_file_async = function (path, cb) {
        try {
            cb(os.file.readFile(path, "utf-8"));
        } catch (e) {
            cb();
        }
    };
    read_line = readline;
    if (typeof console === "undefined") {
        this.console = {};
        this.console.log = print;
    }
    bye = function () { quit(0); };
    put_string = this.console.log;
} else if (typeof(require) !== "undefined") {
    /* Node.js */

    var fs = require('fs');
    read_file_async = function (path, cb) {
        return fs.readFile(path, "utf-8", function (error, str) { return cb(str) });
    };
    bye = function () { process.exit(0); };

    var readline = require('readline');
    var rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        terminal: false,
    });
    rl.on('line', function (data) {
        gInputLines.push(data);
        resume("//line");
    });
    put_string = console.log;
} else if (typeof(snarf) !== "undefined") {
    /* old SpiderMonkey shell */

    read_file_async = function (path, cb) {
        try {
            cb(snarf(path, "utf-8"));
        } catch (e) {
            cb();
        }
    };
    read_line = readline;
    this.console = {};
    this.console.log = print;
    bye = function () { quit(0); };
    put_string = this.console.log;
} else if (typeof(readFile) !== "undefined") {
    /* JavaScriptCore */

    read_file_async = function (path, cb) {
        try {
            cb(readFile(path));
        } catch (e) {
            cb();
        }
    };
    read_line = readline;
    if (typeof console === "undefined") {
        this.console = {};
        this.console.log = print;
    }
    bye = function () { quit(); };
    put_string = this.console.log;
} else if (false && typeof(fetch) !== "undefined") {
    /* Web */

    read_file_async = function (path, cb) {
        fetch(path).then(function(x) { return x.text(); }).then(function(str) { return cb(str); }).catch(function (str) { cb() });
    };
    put_string = function (str) {
        forth_output(str + "\n");
    };
} else {
    /* Web */

    read_file_async = function (path, cb) {
        var req = new XMLHttpRequest();
        req.onreadystatechange = function () {
            if (req.readyState == 4 && req.status == 200)
                cb(req.responseText);
            else if (req.readyState == 4)
                cb();
        };
        req.open("GET", path);
        req.send();
    };
    put_string = function (str) {
        forth_output(str + "\n");
    };
}

var heap = new ArrayBuffer(params.memsize);
var HEAPU8 = new Uint8Array(heap);
var HEAPU32 = new Uint32Array(heap);

/* console I/O */

var gLine = "";

function clog(addr) /* unused? */
{
    put_string(CStringAt(HEAPU8, addr));
}

function foreign_putchar(c)
{
    if (c == 10) {
        put_string(gLine);
        gLine = "";
    } else {
        gLine += String.fromCharCode(c);
    }
}

/* Library functions */

function CStringTo(str, heap, offset)
{
    var i0;

    for (i0 = 0; i0 < str.length; i0++) {
        heap[offset + i0] = str.charCodeAt(i0);
    }

    heap[offset + i0] = 0;

    return i0+1;
}

function CStringAt(heap, offset)
{
    var ret = '';

    for (var i0 = offset; heap[i0]; i0++) {
        ret += String.fromCharCode(heap[i0]);
    }

    return ret;
}

function StringAt(heap, offset, length)
{
    var ret = '';

    for (var i0 = offset; length--; i0++) {
        ret += String.fromCharCode(heap[i0]);
    }

    return ret;
}

var startDate = new Date();

function foreign_bye(c)
{
    bye();
}

function foreign_dump(c)
{
    var s = "";
    for (var i = 0; i < (params.memsize + 3) / 4; i++) {
        if (HEAPU32[i])
            s += "HEAPU32["+i+"] = 0x"+HEAPU32[i].toString(16)+";\n";
    }
    put_string(s);
}

var loaded = {};
var load_address = {};
var next_load_address = params.fsoff;
var load_size = {};

function load_file(heapu8, path)
{
    var str;
    var succ;
    read_file_async(path, function (str) {
        if (str === undefined || str === null) {
            loaded[path] = 1;
            resume(path);
            return;
        }

        next_load_address += 31;
        next_load_address &= -32;
        load_size[path] = CStringTo(str, heapu8, next_load_address + 32);
        load_address[path] = next_load_address;;
        HEAPU32[next_load_address+4>>2] = 0; // position
        HEAPU32[next_load_address+8>>2] = load_size[path]-1; // size
        HEAPU32[next_load_address+12>>2] = 1; // call slow_read flag
        next_load_address += 32 + load_size[path];

        succ = true;
        loaded[path] = 1;
        resume(path);
    });

    if (succ)
        return true;

    resume_string = path;
    return;
}

var fhs = {}; /* file handles */

function foreign_open_file(addr, u, mode)
{
    var path = StringAt(HEAPU8, addr, u);
    var mode = CStringAt(HEAPU8, mode);

    var fileid = 0;

    if (!loaded[path]) {
        loaded[path] = .5;

        load_file(HEAPU8, path);
    }
    if (loaded[path] == .5) {
        resume_string = path;
        return -2;
    }

    if (path in load_address) {
        fileid = load_address[path];
        fhs[fileid] = { offset: 0 };
        HEAPU32[load_address[path]+4>>2] = 0; // reset position.
    }

    return fileid;
}

function foreign_read_file(addr, u1, fileid)
{
    var i;

    if (fileid === 0 && (!fhs[fileid] || HEAPU8[fhs[fileid].offset + 32] === 0)) {
       fhs[0] = { offset: 1023 * 1024 };
       for (var i = 0; i < 1024; i++)
           HEAPU8[1023 * 1024 + i] = 0;
       var str;
       do {
           if (gInputLines.length)
               str = gInputLines.shift();
           else {
               if (read_line)
                   str = read_line();
               else {
                   resume_string = "//line";
                   return -2;
               }
           }
       } while (str === "");

       if (!str)
          foreign_bye(0);
       var len = CStringTo(str, HEAPU8, fhs[0].offset + 32);
       HEAPU8[1024 * 1023 + 32 + len - 1] = "\n".charCodeAt(0);
       HEAPU8[1024 * 1023 + 32 + len] = 0;
    }
    var off = fhs[fileid].offset;

    for (i = 0; i < u1; i++)
        if ((HEAPU8[addr++] = HEAPU8[fileid + off + 32 + i]) == 0)
           break;

    fhs[fileid].offset += i;
    return i;
}

function lbForth(stdlib, foreign, buffer)
{
    "use asm";
    var HEAPU8 = new stdlib.Uint8Array(buffer);
    var HEAPU32 = new stdlib.Uint32Array(buffer);
    var imul = stdlib.Math.imul || function(a, b) {
        var ah = (a >>> 16) & 0xffff;
        var al = a & 0xffff;
        var bh = (b >>> 16) & 0xffff;
        var bl = b & 0xffff;
        return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0)|0);
    };
    var foreign_putchar = foreign.putchar;
    var foreign_open_file = foreign.open_file;
    var foreign_read_file = foreign.read_file;
    var foreign_bye = foreign.bye;
    var foreign_dump = foreign.dump;

function asmmain(word, IP, SP, RP)
{
    word = word|0;
    IP = IP|0;
    SP = SP|0;
    RP = RP|0;
    var addr = 0;
    var x = 0;
    var y = 0;
    var z = 0;
    var c = 0;
    var i = 0;
    var top = 0;

    while (1|0) {
        top = HEAPU32[SP>>2]|0;
        switch (HEAPU32[word+24>>2]|0) {
end-code

code exit
    IP = HEAPU32[RP>>2]|0;
    RP = RP+4|0;
end-code

code sp@
    SP = SP-4|0;
    HEAPU32[SP>>2] = SP+4|0;
end-code

code sp!
    SP = top;
end-code

code rp@
    SP = SP-4|0;
    HEAPU32[SP>>2] = RP|0;
end-code

code rp!
    RP = top;
    SP = SP+4|0;
end-code

code dodoes
    SP = SP-4|0;
    HEAPU32[SP>>2] = word + 28|0;
    RP = RP-4|0;
    HEAPU32[RP>>2] = IP|0;
    IP = HEAPU32[word + 16 + 4 >> 2]|0;
end-code

code docol
    RP = RP-4|0;
    HEAPU32[RP>>2] = IP|0;
    IP = word+28|0;
end-code

code dovar
    SP = SP-4|0;
    HEAPU32[SP>>2] = word+28|0;
end-code

code docon
    SP = SP-4|0;
    HEAPU32[SP>>2] = HEAPU32[word+28>>2];
end-code

code dodef
    word = HEAPU32[word + 28>>2]|0;
    continue;
end-code

code 0branch
    addr = HEAPU32[IP>>2]|0;
    SP = SP+4|0;
    if ((top|0) == 0)
      IP = addr|0;
    else
      IP=IP+4|0;
end-code

code branch
    IP = HEAPU32[IP>>2]|0;
end-code

code (literal)
    SP = SP-4|0;
    HEAPU32[SP>>2] = HEAPU32[IP>>2]|0;
    IP=IP+4|0;
end-code

code !
    SP = SP+4|0;
    x = HEAPU32[SP>>2]|0;
    SP = SP+4|0;
    HEAPU32[top>>2] = x;
end-code

code @
    HEAPU32[SP>>2] = HEAPU32[top>>2]|0;
end-code

code +
    SP = SP+4|0;
    HEAPU32[SP>>2] = (HEAPU32[SP>>2]|0)+top;
end-code

code negate
    HEAPU32[SP>>2] = -top|0;
end-code

code -
    SP = SP+4|0;
    HEAPU32[SP>>2] = ((HEAPU32[SP>>2]|0)|0)-top|0;
end-code

code >r  ( x -- ) ( R: -- x )
    SP = SP+4|0;
    RP = RP - 4|0;
    HEAPU32[RP>>2] = top|0;
end-code

code r> ( -- x ) ( R: x -- )
    x = HEAPU32[RP>>2]|0;
    RP = RP+4|0;
    SP = SP-4|0;
    HEAPU32[SP>>2] = x|0;
end-code

code 2r>
    x = HEAPU32[RP>>2]|0;
    RP = RP+4|0;
    y = HEAPU32[RP>>2]|0;
    RP = RP+4|0;
    SP = SP-4|0;
    HEAPU32[SP>>2] = y|0;
    SP = SP-4|0;
    HEAPU32[SP>>2] = x|0;
end-code

code 2>r
    SP = SP+4|0;
    y = HEAPU32[SP>>2]|0;
    SP = SP+4|0;
    RP = RP-4|0;
    HEAPU32[RP>>2] = y|0;
    RP = RP-4|0;
    HEAPU32[RP>>2] = top|0;
end-code

code c!
    SP = SP+4|0;
    c = HEAPU32[SP>>2]|0;
    SP = SP+4|0;
    HEAPU8[top] = c|0;
end-code

code c@
    HEAPU32[SP>>2] = HEAPU8[top|0]|0;
end-code

code (loop)
    HEAPU32[RP>>2] = (HEAPU32[RP>>2]|0)+1|0;
    SP = SP-4|0;
    if ((HEAPU32[RP>>2]|0) >= (HEAPU32[RP+4>>2]|0))
        HEAPU32[SP>>2] = -1;
    else
        HEAPU32[SP>>2] = 0;
end-code

code 2rdrop
    RP = RP+8|0;
end-code

code emit
    SP = SP+4|0;
    foreign_putchar (top|0)|0;
end-code

\ optional words

code dup
    SP = SP-4|0;
    HEAPU32[SP>>2] = top|0;
end-code

code 0=
    if ((top|0) == 0)
        c = -1;
    else
        c = 0;
    HEAPU32[SP>>2] = c|0;
end-code

code 0<>
    if ((top|0) == 0)
        c = 0;
    else
        c = -1;
    HEAPU32[SP>>2] = c|0;
end-code

code 0<
    if (0 > (top|0))
        c = -1;
    else
        c = 0;
    HEAPU32[SP>>2] = c|0;
end-code

code <
    SP = SP+4|0;
    if ((top>>0) > (HEAPU32[SP>>2]>>0))
        c = -1;
    else
        c = 0;
    HEAPU32[SP>>2] = c|0;
end-code

code rot
    HEAPU32[SP>>2] = HEAPU32[SP+8>>2]|0;
    HEAPU32[SP+8>>2] = HEAPU32[SP+4>>2]|0;
    HEAPU32[SP+4>>2] = top;
end-code

code -rot
    HEAPU32[SP>>2] = HEAPU32[SP+4>>2]|0;
    HEAPU32[SP+4>>2] = HEAPU32[SP+8>>2]|0;
    HEAPU32[SP+8>>2] = top;
end-code

code nip
    SP = SP+4|0;
    HEAPU32[SP>>2] = top;
end-code

code drop
    SP = SP+4|0;
end-code

code 2dup
    SP=SP-8|0;
    HEAPU32[SP+4>>2] = HEAPU32[SP+12>>2]|0;
    HEAPU32[SP>>2] = top;
end-code

code ?dup
    if (top|0) {
        SP = SP-4|0;
        HEAPU32[SP>>2] = top|0;
    }
end-code

code swap
    HEAPU32[SP>>2] = HEAPU32[SP+4>>2]|0;
    HEAPU32[SP+4>>2] = top;
end-code

code over
    SP = SP-4|0;
    HEAPU32[SP>>2] = HEAPU32[SP+8>>2]|0;
end-code

code invert
    HEAPU32[SP>>2] = ~top;
end-code

code xor
    SP=SP+4|0;
    HEAPU32[SP>>2] = HEAPU32[SP>>2]^top;
end-code

code or
    SP=SP+4|0;
    HEAPU32[SP>>2] = HEAPU32[SP>>2]|top;
end-code

code and
    SP=SP+4|0;
    HEAPU32[SP>>2] = HEAPU32[SP>>2]&top;
end-code

code nand
    SP=SP+4|0;
    HEAPU32[SP>>2] = ~(HEAPU32[SP>>2]&top);
end-code

code =
    SP=SP+4|0;
    HEAPU32[SP>>2] = ((HEAPU32[SP>>2]|0) == (top>>0)) ? -1 : 0;
end-code

code <>
    SP=SP+4|0;
    HEAPU32[SP>>2] = ((HEAPU32[SP>>2]|0) != (top>>0)) ? -1 : 0;
end-code

code 1+
    HEAPU32[SP>>2] = top + 1|0;
end-code

code +!
    SP=SP+4|0;
    HEAPU32[top>>2] = (HEAPU32[top>>2]|0)+(HEAPU32[SP>>2]|0)|0;
    SP=SP+4|0;
end-code

code 2*
    HEAPU32[SP>>2] = (top|0) + (top|0)|0;
end-code

code *
    SP=SP+4|0;
    HEAPU32[SP>>2] = imul(top|0, HEAPU32[SP>>2]|0)|0;
end-code

code tuck
    SP=SP-4|0;
    HEAPU32[SP+4>>2] = HEAPU32[SP+8>>2]|0;
    HEAPU32[SP+8>>2] = top|0;
    HEAPU32[SP>>2] = top|0;
end-code

code bye
    foreign_bye(0)|0;
end-code

code close-file
    HEAPU32[SP>>2] = 0;
end-code

code open-file
    SP = SP+4|0;
    y = HEAPU32[SP>>2]|0;
    SP = SP+4|0;
    c = HEAPU32[SP>>2]|0;
    SP = SP+4|0;

    addr = foreign_open_file(c|0, y|0, top|0)|0;
    if ((addr|0) == -2) {
        SP = SP-16|0;
        HEAPU32[SP>>2] = IP|0;
        SP = SP-4|0;
        HEAPU32[SP>>2] = RP|0;
        SP = SP-4|0;
        HEAPU32[SP>>2] = word|0;

        return SP|0;
    }
    SP = SP-4|0;
    HEAPU32[SP>>2] = addr|0;
    SP = SP-4|0;
    if ((addr|0) == 0)
        HEAPU32[SP>>2] = 1|0;
    else
        HEAPU32[SP>>2] = 0;
end-code

code read-file
    c = HEAPU32[SP>>2]|0;
    SP = SP+4|0;
    z = HEAPU32[SP>>2]|0;
    SP = SP+4|0;
    addr = HEAPU32[SP>>2]|0;
    SP = SP+4|0;

    x = HEAPU32[c+8>>2]|0;
    y = HEAPU32[c+4>>2]|0;

    if ((x|0) == (y|0)) {
        if ((HEAPU32[c+12>>2]|0) != 0)
            i = 0;
        else
            i = foreign_read_file(addr|0, z|0, c|0)|0;
        if ((i|0) == -2) {
            SP = SP-16|0;
            HEAPU32[SP>>2] = IP|0;
            SP = SP-4|0;
            HEAPU32[SP>>2] = RP|0;
            SP = SP-4|0;
            HEAPU32[SP>>2] = word|0;

            return SP|0;
        }
    } else {
        if ((z>>>0) > ((x-y)>>>0))
            z = (x-y)|0;
        for (i = 0; (i>>>0) < (z>>>0); i = (i+1)|0) {
            HEAPU8[(addr+i)|0] = HEAPU8[(c+32+y+i)|0]|0;
        }
        HEAPU32[c+4>>2] = (y + i)|0;
    }

    SP = SP-4|0;
    HEAPU32[SP>>2] = i|0;
    SP = SP-4|0;
    HEAPU32[SP>>2] = 0;
end-code

start-code
        }
        word = HEAPU32[IP>>2]|0;
        IP = IP + 4|0;
    }

    return 0;
}

    return { asmmain: asmmain };
}

var asmmodule;
var global_sp;

function run(turnkey)
{
    asmmodule = lbForth({
            Uint8Array: Uint8Array,
            Uint32Array: Uint32Array,
            Math: {
                imul: Math.imul || function(a, b) {
                    var ah = (a >>> 16) & 0xffff;
                    var al = a & 0xffff;
                    var bh = (b >>> 16) & 0xffff;
                    var bl = b & 0xffff;
                    return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0)|0);
                }
            }
        }, {
            clog: clog,
            putchar: foreign_putchar,
            open_file: foreign_open_file,
            read_file: foreign_read_file,
            bye: foreign_bye,
            dump: foreign_dump
        }, heap);

    try {
        return global_sp = asmmodule.asmmain(turnkey, params.dictoff, params.sp0, params.rp0);
    } catch (e) {
        put_string(e);
    }
}

function resume(str)
{
    if (str !== resume_string)
        return;

    if (str === undefined)
        return;

    resume_string = undefined;

    var sp = global_sp;
    if (!global_sp)
        return;

    var word = HEAPU32[sp>>2];
    sp += 4;
    var RP = HEAPU32[sp>>2];
    sp += 4;
    var IP = HEAPU32[sp>>2];
    sp += 4;
    var SP = sp;

    try {
        global_sp = 0;
        global_sp = asmmodule.asmmain(word, IP, SP, RP);
    } catch (e) {
        put_string(e);
    }
}
end-code
