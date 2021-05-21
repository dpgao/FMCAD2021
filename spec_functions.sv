/*
MIT License

Copyright (c) 2021 Dapeng Gao

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

typedef logic signed [127 : 0] clen;
typedef logic signed [65 : 0] llen;
typedef logic signed [63 : 0] xlen;
typedef logic signed [31 : 0] word;
typedef logic [22 : 0] dii_id;
typedef logic signed [15 : 0] half;

let min(a, b) = a < b ? a : b;

typedef struct packed {
    logic [3 : 0] uperms;
    logic setCID;
    logic sysRegs;
    logic unseal;
    logic cinvoke;
    logic seal;
    logic storeLocalCap;
    logic storeCap;
    logic loadCap;
    logic store;
    logic load;
    logic execute;
    logic globalCap;
} perm;

typedef struct packed {

/*
typedef struct {
  Bool           isCapability;                                                  151
  Bit#(CapAddrW) address;                                                       150: 87
  Bit#(MW)       addrBits;                                                      86 : 73
  Perms          perms;                                                         72 : 57
  Bit#(FlagsW)   flags;                                                         56
  Bit#(ResW)     reserved;                                                      55 : 54
  Bit#(OTypeW)   otype;                                                         53 : 36
  Format         format;                                                        35
  Bounds         bounds;                                                        34 :  0
} CapFat deriving(Bits);
*/

    logic isCapability;
    xlen addr;
    logic [13 : 0] addrBits;
    perm perms;
    logic flags;
    logic [1 : 0] reserved;
    logic [17 : 0] otype;
    logic format;
    struct packed {

/*
typedef struct
{
    Exp exp;
    Bit#(MW) topBits;
    Bit#(MW) baseBits;
    Bool overflow;
} Bounds deriving (Bits, Eq, FShow);
*/

        logic [5 : 0] exp;
        logic [13 : 0] topBits, baseBits;
        logic overflow;
    } bounds;
} cap;

wire cap almighty_cap = '{
    isCapability: '1,
    perms: '1,
    otype: '1,
    format: '1,
    bounds: '{
        exp: 52,
        topBits: {2'b1, 12'b0},
        default: '0
    },
    default: '0
};

function automatic logic hasReservedOType(cap c);
    return c.otype > ~18'd4;
endfunction

function automatic logic [1 : 0] topBitsMSB(logic format, logic [11 : 0] topBits, logic [13 : 0] baseBits);
    return baseBits[13 : 12] + format + (format ?
        topBits[11 : 3] < baseBits[11 : 3] :
        topBits[11 : 0] < baseBits[11 : 0]);
endfunction

function automatic logic [13 : 0] getAddrBits(xlen addr, logic [5 : 0] e);
    return {addr >> e}[13 : 0];
endfunction

function automatic logic isDerivable(logic [5 : 0] e, logic [13 : 0] baseBits);
    return (e == 52 && baseBits[13 : 12] == '0)
        || (e == 51 && baseBits[13] == '0)
        || (e <= 50);
endfunction

function automatic logic consistent(cap c);
    return (c.isCapability -> c.reserved == '0 && !c.bounds.overflow)
        && c.addrBits == getAddrBits(c.addr, c.bounds.exp)
        && (c.format ?
                c.bounds.baseBits[2 : 0] == '0 && c.bounds.topBits[2 : 0] == '0 && !c.bounds.overflow :
                c.bounds.exp == '0)
        && c.bounds.topBits[13 : 12] == topBitsMSB(c.format, c.bounds.topBits[11 : 0], c.bounds.baseBits)
        && (c.bounds.overflow -> !isDerivable({c.bounds.topBits[2 : 0], c.bounds.baseBits[2 : 0]}, c.bounds.baseBits))
        && isDerivable(c.bounds.exp, c.bounds.baseBits);
endfunction

function automatic cap embedInt(xlen addr);
    return '{
        addr: addr,
        addrBits: {2'b0, addr[63 : 52]},
        otype: '1,
        format: '1,
        bounds: '{
            exp: 52,
            topBits: {2'b1, 12'b0},
            default: '0
        },
        default: '0
    };
endfunction

function automatic cap setTag(cap c, logic tag);
    c.isCapability = tag;
    return c;
endfunction

function automatic cap setPointer(cap c, xlen addr);
    c.addr = addr;
    c.addrBits = getAddrBits(addr, c.bounds.exp);
    return c;
endfunction

function automatic cap setPerms(cap c, perm perms);
    c.perms = perms;
    return c;
endfunction

function automatic cap setFlags(cap c, xlen flags);
    c.flags = flags[0];
    return c;
endfunction

function automatic cap setOType(cap c, logic [17 : 0] otype);
    c.otype = otype;
    return c;
endfunction




typedef struct packed {

/*
typedef struct {
  Bit#(3)   repBoundTopBits;
  Bool      topHi;
  Bool      baseHi;
  Bool      addrHi;
  Int#(2)   topCorrection;
  Int#(2)   baseCorrection;
} MetaInfo deriving(Bits, Eq, FShow);
*/

    logic [2 : 0] repBoundTopBits;
    logic topHi;
    logic baseHi;
    logic addrHi;
    logic [1 : 0] topCorrection;
    logic [1 : 0] baseCorrection;
} meta_info;

function automatic meta_info getMeta(cap c);
    logic [2 : 0]
        tb = c.bounds.topBits[13 : 11],
        bb = c.bounds.baseBits[13 : 11],
        ab = c.addrBits[13 : 11];
    logic [2 : 0] repBound = bb - 3'b1;
    logic
        topHi = tb < repBound,
        baseHi = bb < repBound,
        addrHi = ab < repBound;
    return '{
        repBoundTopBits: repBound,
        topHi:  topHi,
        baseHi: baseHi,
        addrHi: addrHi,
        topCorrection:  {1'b0, topHi} - addrHi,
        baseCorrection: {1'b0, baseHi} - addrHi
    };
endfunction




typedef struct packed {
    cap cap;
    meta_info meta;
} cap_pipe;

function automatic logic pipeConsistent(cap_pipe pipe);
    return consistent(pipe.cap) && pipe.meta == getMeta(pipe.cap);
endfunction




typedef struct {
    llen base;
    llen top;
} ubound;

typedef struct {
    xlen base;
    llen top;
} bound;

function automatic ubound getCapBoundsBitsUncorrected(cap c);
    logic [5 : 0] e = min(c.bounds.exp, 6'd52);
    llen addrTop = {2'b0, c.addr & ({~50'b0, 14'b0} << e)};
    return '{
        base: addrTop | (c.bounds.baseBits << e),
        top: addrTop | (c.bounds.topBits << e)
    };
endfunction

function automatic cap getCapLegalised(cap c);
    ubound b = getCapBoundsBitsUncorrected(c);
    if (c.bounds.exp > 52 || b.base[65 : 64]) begin
        c.isCapability = '0;
        c.format = '0;
        {c.bounds.topBits[2 : 0], c.bounds.baseBits[2 : 0]} = c.bounds.exp;
        c.bounds.topBits[13 : 12] = topBitsMSB(c.format, c.bounds.topBits[11 : 0], c.bounds.baseBits);
        c.bounds.exp = '0;
        c.addrBits = getAddrBits(c.addr, c.bounds.exp);
    end
    return c;
endfunction

function automatic bound getCapBounds(cap in);
    cap c = getCapLegalised(in);
    logic [5 : 0] e = c.bounds.exp;
    ubound b = getCapBoundsBitsUncorrected(c);
    meta_info meta = getMeta(c);
    llen
        base = b.base + (signed'({meta.baseCorrection, 14'b0}) << e),
        top = b.top + (signed'({meta.topCorrection, 14'b0}) << e);
    top[65 : 64] -= base[65 : 64];
    return '{
        base: xlen'(base),
        top: top
    };
endfunction

function automatic xlen getCapBase(cap c);
    return getCapBounds(c).base;
endfunction

function automatic llen getCapTop(cap c);
    return getCapBounds(c).top;
endfunction

function automatic xlen getCapLength(cap c);
    bound b = getCapBounds(c);
    llen length = b.top - unsigned'(b.base);
    return length[65 : 64] ? '1 : xlen'(length);
endfunction

function automatic logic inCapBounds(cap c, xlen addr, llen top);
    bound b = getCapBounds(c);
    return (unsigned'(b.base) <= unsigned'(addr))
        && (unsigned'(top) <= unsigned'(b.top));
endfunction

function automatic xlen getCapOffset(cap c);
    return c.addr - getCapBase(c);
endfunction




typedef struct {
    logic flag;
    cap result;
} bresult;

function automatic logic fastRepCheck(cap in, xlen inc);
    cap c = getCapLegalised(in);
    meta_info meta = getMeta(c);
    logic [5 : 0] e = c.bounds.exp;
    logic [13 : 0]
        iMid = getAddrBits(inc, e),
        repBound = {meta.repBoundTopBits, 11'b0};
    logic [13 : 0] toBoundsN = repBound - c.addrBits;
    logic [13 : 0] toBoundsP = toBoundsN - 14'b1;
    logic [49 : 0] iTop = signed'(inc[63 : 14]) >>> e;
    if (e >= 6'd50)
        return '1;
    else if (iTop == '0)
        return iMid < toBoundsP;
    else if (iTop == '1)
        return iMid >= toBoundsN && c.addrBits != repBound;
    else
        return '0;
endfunction

function automatic logic fasterRepCheck(cap in, xlen off);
    cap c = getCapLegalised(in);
    logic [5 : 0] e = c.bounds.exp;
    logic [13 : 0]
        iMid = getAddrBits(off, e);
    logic [13 : 0] toBoundsN = {3'b111, 11'b0} - {3'b0, c.bounds.baseBits[10 : 0]};
    logic [13 : 0] toBoundsP = toBoundsN - 14'b1;
    logic [49 : 0] iTop = signed'(off[63 : 14]) >>> e;
    if (e >= 6'd50)
        return '1;
    else if (iTop == '0)
        return iMid <= toBoundsP;
    else if (iTop == '1)
        return iMid >= toBoundsN;
    else
        return '0;
endfunction

function automatic bresult incCapOffset(cap in, xlen inc);
    xlen newAddr = in.addr + inc;
    cap c = setPointer(in, newAddr);
    return '{
        flag: fastRepCheck(in, inc),
        result: c
    };
endfunction

function automatic bresult setCapOffset(cap in, xlen off);
    xlen newAddr = getCapBase(in) + off;
    cap c = setPointer(in, newAddr);
    return '{
        flag: fasterRepCheck(in, off),
        result: c
    };
endfunction

function automatic bresult setCapAddr(cap c, xlen addr);
    bound oldB = getCapBounds(c);
    c = setPointer(c, addr);
    return '{
        flag: oldB == getCapBounds(c),
        result: c
    };
endfunction

function automatic bresult setCapBounds(cap c, xlen base, llen top);
    llen length = top - unsigned'(base);
    logic [5 : 0] e =
        (length[64 : 13] & (1 << 51)) ? 6'd52 : (length[64 : 13] & (1 << 50)) ? 6'd51 :
        (length[64 : 13] & (1 << 49)) ? 6'd50 : (length[64 : 13] & (1 << 48)) ? 6'd49 :
        (length[64 : 13] & (1 << 47)) ? 6'd48 : (length[64 : 13] & (1 << 46)) ? 6'd47 :
        (length[64 : 13] & (1 << 45)) ? 6'd46 : (length[64 : 13] & (1 << 44)) ? 6'd45 :
        (length[64 : 13] & (1 << 43)) ? 6'd44 : (length[64 : 13] & (1 << 42)) ? 6'd43 :
        (length[64 : 13] & (1 << 41)) ? 6'd42 : (length[64 : 13] & (1 << 40)) ? 6'd41 :
        (length[64 : 13] & (1 << 39)) ? 6'd40 : (length[64 : 13] & (1 << 38)) ? 6'd39 :
        (length[64 : 13] & (1 << 37)) ? 6'd38 : (length[64 : 13] & (1 << 36)) ? 6'd37 :
        (length[64 : 13] & (1 << 35)) ? 6'd36 : (length[64 : 13] & (1 << 34)) ? 6'd35 :
        (length[64 : 13] & (1 << 33)) ? 6'd34 : (length[64 : 13] & (1 << 32)) ? 6'd33 :
        (length[64 : 13] & (1 << 31)) ? 6'd32 : (length[64 : 13] & (1 << 30)) ? 6'd31 :
        (length[64 : 13] & (1 << 29)) ? 6'd30 : (length[64 : 13] & (1 << 28)) ? 6'd29 :
        (length[64 : 13] & (1 << 27)) ? 6'd28 : (length[64 : 13] & (1 << 26)) ? 6'd27 :
        (length[64 : 13] & (1 << 25)) ? 6'd26 : (length[64 : 13] & (1 << 24)) ? 6'd25 :
        (length[64 : 13] & (1 << 23)) ? 6'd24 : (length[64 : 13] & (1 << 22)) ? 6'd23 :
        (length[64 : 13] & (1 << 21)) ? 6'd22 : (length[64 : 13] & (1 << 20)) ? 6'd21 :
        (length[64 : 13] & (1 << 19)) ? 6'd20 : (length[64 : 13] & (1 << 18)) ? 6'd19 :
        (length[64 : 13] & (1 << 17)) ? 6'd18 : (length[64 : 13] & (1 << 16)) ? 6'd17 :
        (length[64 : 13] & (1 << 15)) ? 6'd16 : (length[64 : 13] & (1 << 14)) ? 6'd15 :
        (length[64 : 13] & (1 << 13)) ? 6'd14 : (length[64 : 13] & (1 << 12)) ? 6'd13 :
        (length[64 : 13] & (1 << 11)) ? 6'd12 : (length[64 : 13] & (1 << 10)) ? 6'd11 :
        (length[64 : 13] & (1 <<  9)) ? 6'd10 : (length[64 : 13] & (1 <<  8)) ? 6'd9  :
        (length[64 : 13] & (1 <<  7)) ? 6'd8  : (length[64 : 13] & (1 <<  6)) ? 6'd7  :
        (length[64 : 13] & (1 <<  5)) ? 6'd6  : (length[64 : 13] & (1 <<  4)) ? 6'd5  :
        (length[64 : 13] & (1 <<  3)) ? 6'd4  : (length[64 : 13] & (1 <<  2)) ? 6'd3  :
        (length[64 : 13] & (1 <<  1)) ? 6'd2  : (length[64 : 13] & (1 <<  0)) ? 6'd1  :
        6'd0;
    logic format = (e != 0) || length[12];
    logic [13 : 0]
        topBits = top[13 : 0],
        baseBits = base[13 : 0];
    logic
        lostSignificantTop = '0,
        lostSignificantBase = '0;
    if (format) begin
        logic [10 : 0]
            topIE = {top >> e}[13 : 3],
            baseIE = {base >> e}[13 : 3];
        logic [54 : 0] mask = {~{~52'b0 << e}, 3'b111};
        lostSignificantTop = (top[54 : 0] & mask) != '0;
        lostSignificantBase = (base[54 : 0] & mask) != '0;
        if (lostSignificantTop)
            topIE += 11'b1;
        if ({topIE - baseIE}[10]) begin
            e += 6'b1;
            lostSignificantBase |= baseIE[0];
            lostSignificantTop |= topIE[0];
            baseIE = {base >> e}[13 : 3];
            topIE = {top >> e}[13 : 3] + lostSignificantTop;
        end
        baseBits = {baseIE, 3'b0};
        topBits = {topIE, 3'b0};
    end
    c.addr = base;
    c.addrBits = getAddrBits(base, e);
    c.format = format;
    c.bounds.exp = e;
    c.bounds.topBits = topBits;
    c.bounds.baseBits = baseBits;
    return '{
        flag: !(lostSignificantBase || lostSignificantTop),
        result: c
    };
endfunction




typedef struct packed {
    cap_pipe cap_pipe;
    xlen base;
} pcc;

function automatic logic pccConsistent(pcc pcc);
    return pipeConsistent(pcc.cap_pipe);
        /* && pcc.base == getCapBase(pcc.cap_pipe.cap) */
        /* && pcc.cap_pipe.cap.otype == '1; */
endfunction




typedef struct packed {

/*
typedef struct {
   PCC_T epcc;
   CHERI_Exc_Code cheri_exc_code;
   Bit#(6) cheri_exc_reg;
   Exc_Code  exc_code;
   Addr      tval;
   } Trap_Info_Pipe
deriving (Bits, FShow);
*/

    pcc epcc;
    logic [4 : 0] cheri_exc_code;
    logic [5 : 0] cheri_exc_reg;
    logic [5 : 0] exc_code;
    xlen tval;
} trap_info;




typedef struct {
    logic [1 : 0] op;
    logic ignore_result;
    cap result;

    logic check_enable;
    logic check_exact;
    logic check_inclusive;

    logic exc;
    logic check;
    logic exact;
    logic mem_allow_cap;

    cap authority;
    xlen low;
    llen high;
} retire;




function automatic retire cAUIPCC(cap c, xlen inc);
    bresult r = incCapOffset(c, inc); // TODO
    r.result.isCapability &= r.flag;
    return '{
        result: r.result,
        default: '0
    };
endfunction




function automatic xlen getRepresentableLength(xlen len);
    xlen mask = getRepresentableAlignmentMask(len);
    return (len + ~mask) & mask;
endfunction

function automatic xlen getRepresentableAlignmentMask(xlen len);
    cap c = setCapBounds(almighty_cap, '0, unsigned'(len)).result;
    logic [5 : 0] e = min(c.bounds.exp, 6'd52);
    if (c.format)
        return {~61'b0, 3'b0} << e;
    else
        return '1 << e;
endfunction

function automatic retire cJALR(cap c1, cap pcc);
    xlen
        newPC = c1.addr & ~1,
        newPCCBase = getCapBase(c1);
    llen newPCHigh = unsigned'(newPC) + 2;
    bresult r = incCapOffset(pcc, 64'd4); // TODO
    r.result.otype = ~18'd1;
    return '{
        result: r.result,

        check_enable: '1,
        check_inclusive: '1,

        exc: !c1.isCapability || (c1.otype != '1 && c1.otype != ~18'd1)
          || !c1.perms.execute
          || newPC[0]
          || newPCCBase[1 : 0] != '0
          || pcc.otype != '1
          || !r.flag,
        check: !inCapBounds(c1, newPC, newPCHigh),

        authority: c1,
        low: newPC,
        high: newPCHigh,
        default: '0
    };
endfunction

function automatic retire cSealEntry(cap c);
    return '{
        result: setOType(c, ~18'd1),

        exc: !c.isCapability || c.otype != '1
          || !c.perms.execute,
        default: '0
    };
endfunction




function automatic retire cSetBounds(cap c, xlen len);
    llen top = unsigned'(c.addr) + unsigned'(len);
    bresult r = setCapBounds(c, c.addr, top);
    return '{
        result: r.result,

        check_enable: '1,
        check_inclusive: '1,

        exc: !c.isCapability || c.otype != '1,
        check: !inCapBounds(c, c.addr, top),

        authority: c,
        low: c.addr,
        high: top,
        default: '0
    };
endfunction

function automatic retire cSetBoundsExact(cap c, xlen len);
    llen top = unsigned'(c.addr) + unsigned'(len);
    bresult r = setCapBounds(c, c.addr, top);
    return '{
        result: r.result,

        check_enable: '1,
        check_inclusive: '1,
        check_exact: '1,

        exc: !c.isCapability || c.otype != '1,
        check: !inCapBounds(c, c.addr, top),
        exact: r.flag,

        authority: c,
        low: c.addr,
        high: top,
        default: '0
    };
endfunction

function automatic retire cSeal(cap c1, cap c2);
    bound b = getCapBounds(c2);
    return '{
        result: setOType(c1, c2.addr[17 : 0]),

        check_enable: '1,

        exc: !c1.isCapability || c1.otype != '1
          || !c2.isCapability || c2.otype != '1
          || !c2.perms.seal
          || unsigned'(c2.addr) > unsigned'(~18'd4),
        check: unsigned'(c2.addr) < unsigned'(b.base)
            || unsigned'(c2.addr) >= unsigned'(b.top),

        authority: c2,
        low: c2.addr,
        high: unsigned'(c2.addr),
        default: '0
    };
endfunction

function automatic retire cUnseal(cap c1, cap c2);
    bound b = getCapBounds(c2);
    cap r = setOType(c1, '1);
    r.perms.globalCap &= c2.perms.globalCap;
    return '{
        result: r,

        check_enable: '1,

        exc: !c1.isCapability || c1.otype == '1
          || !c2.isCapability || c2.otype != '1
          || hasReservedOType(c1)
          || c1.otype != c2.addr
          || !c2.perms.unseal,
        check: unsigned'(c2.addr) < unsigned'(b.base)
            || unsigned'(c2.addr) >= unsigned'(b.top),

        authority: c2,
        low: c2.addr,
        high: unsigned'(c2.addr),
        default: '0
    };
endfunction

function automatic retire cSetOffset(cap c, xlen off);
    bresult r = setCapOffset(c, off);
    r.result.isCapability &= r.flag;
    return '{
        result: r.result,

        exc: c.isCapability && c.otype != '1,
        default: '0
    };
endfunction

function automatic retire cSetAddr(cap c, xlen addr);
    bresult r = setCapAddr(c, addr);
    r.result.isCapability &= r.flag;
    return '{
        result: r.result,

        exc: c.isCapability && c.otype != '1,
        default: '0
    };
endfunction

function automatic retire cIncOffset(cap c, xlen inc);
    bresult r = incCapOffset(c, inc);
    r.result.isCapability &= r.flag;
    return '{
        result: r.result,

        exc: c.isCapability && c.otype != '1,
        default: '0
    };
endfunction

function automatic retire cFromPtr(cap c, xlen off);
    if (off) begin
        bresult r = setCapOffset(c, off);
        r.result.isCapability &= r.flag;
        return '{
            result: r.result,

            exc: !c.isCapability || c.otype != '1,
            default: '0
        };
    end else
        return '{
            result: embedInt('0),
            default: '0
        };
endfunction

function automatic retire cBuildCap(cap c1, cap c2);
    bound
        b1 = getCapBounds(c1),
        b2 = getCapBounds(c2);
    bresult r = setCapBounds(c1, b2.base, b2.top);
    r.result = setCapOffset(r.result, getCapOffset(c2)).result;
    r.result.perms = c2.perms;
    r.result.flags = c2.flags;
    if (c2.otype == ~18'd1)
        r.result.otype = ~18'd1;
    return '{
        result: r.result,

        check_enable: '1,
        check_inclusive: '1,

        exc: !c1.isCapability || c1.otype != '1
          || (c2.perms & c1.perms) != c2.perms,
        check: unsigned'(b2.base) < unsigned'(b1.base)
            || unsigned'(b2.top[64 : 0]) > unsigned'(b1.top[64 : 0]) // TODO
            || unsigned'(b2.base) > unsigned'(b2.top)
            || !r.flag,

        authority: c1,
        low: b2.base,
        high: b2.top,
        default: '0
    };
endfunction

function automatic retire cCopyType(cap c1, cap c2);
    if (hasReservedOType(c2))
        return '{
            result: embedInt(signed'(c2.otype)),

            exc: !c1.isCapability || c1.otype != '1,
            default: '0
        };
    else begin
        bound b = getCapBounds(c1);
        bresult r = setCapAddr(c1, c2.otype);
        r.result.isCapability &= r.flag;
        return '{
            result: r.result,

            check_enable: '1,

            exc: !c1.isCapability || c1.otype != '1,
            check: c2.otype < unsigned'(b.base)
                || c2.otype >= unsigned'(b.top)
                || !r.flag,

            authority: c1,
            low: c2.otype,
            high: c2.otype,
            default: '0
        };
    end
endfunction

function automatic retire cCSeal(cap c1, cap c2);
    bound b = getCapBounds(c2);
    if (!c2.isCapability
     || c1.otype != '1
     || unsigned'(c2.addr) < unsigned'(b.base)
     || unsigned'(c2.addr) >= unsigned'(b.top)
     || c2.addr == '1)
        return '{
            result: c1,

            exc: !c1.isCapability,
            default: '0
        };
    else
        return '{
            result: setOType(c1, c2.addr[17 : 0]),

            check_enable: '1,

            exc: !c1.isCapability
              || c2.otype != '1
              || !c2.perms.seal
              || unsigned'(c2.addr) > unsigned'(~18'd4),

            authority: c2,
            low: c2.addr,
            high: unsigned'(c2.addr),
            default: '0
        };
endfunction

function automatic retire cTestSubset(cap c1, cap c2);
    bound
        b1 = getCapBounds(c1),
        b2 = getCapBounds(c2);
    logic exc = c1.isCapability == c2.isCapability
             && (c2.perms & c1.perms) == c2.perms;
    return '{
        op: exc ? 2'd3 : 2'd0,
        result: embedInt(exc
                      && unsigned'(b2.base) >= unsigned'(b1.base)
                      && unsigned'(b2.top[64 : 0]) <= unsigned'(b1.top[64 : 0])), // TODO
        default: '0
    };
endfunction

function automatic retire cInvoke(cap c1, cap c2);
    xlen
        newPC = c1.addr & ~1,
        newPCCBase = getCapBase(c1);
    llen newPCHigh = unsigned'(newPC) + 2;
    return '{
        result: setOType(c2, '1),

        check_enable: '1,
        check_inclusive: '1,

        exc: !c1.isCapability || hasReservedOType(c1)
          || !c2.isCapability || hasReservedOType(c2)
          || c1.otype != c2.otype
          || !c1.perms.cinvoke || !c2.perms.cinvoke
          || !c1.perms.execute || c2.perms.execute
          || newPC[0]
          || newPCCBase[1 : 0] != '0,
        check: !inCapBounds(c1, newPC, newPCHigh),

        authority: c1,
        low: newPC,
        high: newPCHigh,
        default: '0
    };
endfunction

function automatic retire cLoad(input [2 : 0] width, input cap_or_ddc, cap ddc, cap c1, logic signed [11 : 0] imm);
    cap auth = cap_or_ddc ? c1 : ddc;
    xlen addr = (cap_or_ddc ? 0 : ddc.addr) + c1.addr + imm;
    llen top = unsigned'(addr) + (1 << width);
    return '{
        op: 2'd1,
        ignore_result: '1,

        check_enable: '1,
        check_inclusive: '1,

        exc: !auth.isCapability || auth.otype != '1
          || !auth.perms.load,
        check: !inCapBounds(auth, addr, top),
        mem_allow_cap: width == 3'b100 && auth.perms.loadCap,

        authority: auth,
        low: addr,
        high: top,
        default: '0
    };
endfunction

function automatic retire cStore(input [2 : 0] width, input cap_or_ddc, cap ddc, cap c1, cap c2, logic signed [11 : 0] imm);
    cap auth = cap_or_ddc ? c1 : ddc;
    xlen addr = (cap_or_ddc ? 0 : ddc.addr) + c1.addr + imm;
    llen top = unsigned'(addr) + (1 << width);
    return '{
        op: 2'd2,
        ignore_result: '1,

        check_enable: '1,
        check_inclusive: '1,

        exc: !auth.isCapability || auth.otype != '1
          || !auth.perms.store
          || (width == 3'b100 && (
              (!auth.perms.storeCap && c2.isCapability)
           || (!auth.perms.storeLocalCap && c2.isCapability && !c2.perms.globalCap)
           )),
        check: !inCapBounds(auth, addr, top),

        authority: auth,
        low: addr,
        high: top,
        default: '0
    };
endfunction




typedef struct packed {
    logic tag;
    perm perms;
    logic [1 : 0] reserved;
    logic flags;
    logic [17 : 0] otype;
    logic internal_E;
    logic [11 : 0] T;
    logic [13 : 0] B;
    xlen address;
} ccap;

function automatic cap decompress(ccap m);
    logic [5 : 0] exp = {m.T[2 : 0], m.B[2 : 0]};
    logic overflow = m.internal_E && !isDerivable(exp, m.B);
    logic format = m.internal_E && !overflow;
    logic [5 : 0] e = format ? exp : '0;
    return '{
        isCapability: m.tag,
        addr: m.address,
        addrBits: getAddrBits(m.address, e),
        perms: m.perms,
        flags: m.flags,
        reserved: m.reserved,
        otype: m.otype,
        format: format,
        bounds: '{
            exp: e,
            topBits: {
                topBitsMSB(format, m.T, m.B),
                format ? {m.T[11 : 3], 3'b0} : m.T
            },
            baseBits: format ? {m.B[13 : 3], 3'b0} : m.B,
            overflow: overflow
        }
    };
endfunction

function automatic ccap compress(cap c);
    return '{
        tag: c.isCapability,
        perms: c.perms,
        reserved: c.reserved,
        flags: c.flags,
        otype: c.otype,
        internal_E: c.format || c.bounds.overflow,
        T: c.format ? {c.bounds.topBits[11 : 3], c.bounds.exp[5 : 3]} : c.bounds.topBits[11 : 0],
        B: c.format ? {c.bounds.baseBits[13 : 3], c.bounds.exp[2 : 0]} : c.bounds.baseBits,
        address: c.addr
    };
endfunction
