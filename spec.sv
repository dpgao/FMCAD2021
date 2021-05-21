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

module spec(

    input RST_N,
    input CLK,

    input [3 : 0] cpu_state,
    input [1 : 0] gpr_state,
    input csr_state,

    input cap_pipe next_pcc,
    input cap_pipe ddc,
    input cap_pipe csr_val1,
    input trap_info trap_info,
    input cap regfile[0 : 31],
    input cap mtcc,
    input cap mtdc,
    input cap mscratchc,
    input cap mepcc,
    input pcc s1_pcc,

    input sf_enable,
    input sd_enable,
    input s1_enable,
    input s2_enable,
    input s3_enable,
    input rg_enable,

    input sf_full,
    input sd_full,
    input s1_full,
    input s2_full,
    input s3_full,

    input m_enable,
    input m_store,
    input xlen m_addr,
    input [2 : 0] m_f3,
    input m_unsigned,
    input m_valid,
    input m_tag,
    input clen m_word128,
    input ccap m_store_value,
    input m_exc,

    input EN_rvfi_dii_server_inst_put,
    input [54 : 0] rvfi_dii_server_inst_put,
    input RDY_rvfi_dii_server_seqReq_get,
    input dii_id rvfi_dii_server_seqReq_get,

    input stageF_f_reset_reqs$EMPTY_N,
    input stageF_f_reset_reqs$FULL_N,
    input stageF_f_reset_rsps$EMPTY_N,
    input stageF_f_reset_rsps$FULL_N,

    input stageD_f_reset_reqs$EMPTY_N,
    input stageD_f_reset_reqs$FULL_N,
    input stageD_f_reset_rsps$EMPTY_N,
    input stageD_f_reset_rsps$FULL_N,

    input stage1_f_reset_reqs$EMPTY_N,
    input stage1_f_reset_reqs$FULL_N,
    input stage1_f_reset_rsps$EMPTY_N,
    input stage1_f_reset_rsps$FULL_N,

    input stage2_f_reset_reqs$EMPTY_N,
    input stage2_f_reset_reqs$FULL_N,
    input stage2_f_reset_rsps$EMPTY_N,
    input stage2_f_reset_rsps$FULL_N,

    input stage3_f_reset_reqs$EMPTY_N,
    input stage3_f_reset_reqs$FULL_N,
    input stage3_f_reset_rsps$EMPTY_N,
    input stage3_f_reset_rsps$FULL_N,

    input [259 : 0] stageD_rg_data$D_IN,
    input [453 : 0] stage1_rg_stage_input$D_IN,
    input [1607 : 0] stage2_rg_stage2$D_IN,
    input [952 : 0] stage3_rg_stage3$D_IN,

    input [259 : 0] stageD_rg_data,
    input [453 : 0] stage1_rg_stage_input,
    input [1607 : 0] stage2_rg_stage2,
    input [952 : 0] stage3_rg_stage3
);

default clocking @(posedge CLK); endclocking

sd sf_out(stageD_rg_data$D_IN);
sd sd_reg(stageD_rg_data);

s1 sd_out(stage1_rg_stage_input$D_IN, s1_pcc);
s1 s1_reg(stage1_rg_stage_input, s1_pcc);

s2 s1_out(stage2_rg_stage2$D_IN);
s2 s2_reg(stage2_rg_stage2);

s3 s2_out(stage3_rg_stage3$D_IN);
s3 s3_reg(stage3_rg_stage3);

sequence stall_s2;
    (  !s2_enable
    && !s3_enable)[*]
##1
    s3_enable;
endsequence

function cap s3c(input [4 : 0] r);
    return (r == '0) ? embedInt('0) : regfile[r];
endfunction

function cap s2c(input [4 : 0] r);
    return (r == '0) ? embedInt('0) :
           (s3_full && s3_reg.rd_valid && s3_reg.rd == r) ? s3_reg.rd_val :
           regfile[r];
endfunction

function cap s1c(input [4 : 0] r);
    return (r == '0) ? embedInt('0) :
           (s2_full && s2_out.rd_valid && s2_out.rd == r) ? s2_out.rd_val :
           (s3_full && s3_reg.rd_valid && s3_reg.rd == r) ? s3_reg.rd_val :
           regfile[r];
endfunction

wire cap
    s1c1 = s1c(s1_reg.ins.r1),
    s1c2 = s1c(s1_reg.ins.r2),
    s2c1 = s2c(s2_reg.ins.r1),
    s2c2 = s2c(s2_reg.ins.r2),
    s3c1 = s3c(s3_reg.ins.r1),
    s3c2 = s3c(s3_reg.ins.r2);

wire ccap
    m_in = m_store_value ^ compress(embedInt('0)),
    m_out = {m_tag, m_word128} ^ compress(embedInt('0));

dii_id req_seq;
logic s1_latch, s2_latch, s3_latch;
always_latch begin
    if (RDY_rvfi_dii_server_seqReq_get) req_seq = rvfi_dii_server_seqReq_get;
    if (~RST_N || stage1_f_reset_rsps$EMPTY_N) s1_latch = '0; else if (s1_full) s1_latch = '1;
    if (~RST_N || stage2_f_reset_rsps$EMPTY_N) s2_latch = '0; else if (s2_full) s2_latch = '1;
    if (~RST_N || stage3_f_reset_rsps$EMPTY_N) s3_latch = '0; else if (s3_full) s3_latch = '1;
end

mem_consistent: assume property (
    m_valid && !m_exc && m_out.tag
|->
    m_out.reserved == '0 && !decompress(m_out).bounds.overflow
);

dii_consistent: assume property (
    EN_rvfi_dii_server_inst_put
|->
    rvfi_dii_server_inst_put[24 : 0] == {2'b11, req_seq}
);

decode: assert property (
    s1_latch
|->
       s1_reg.i32
    && s1_reg.dins[161 : 155] == s1_reg.ins.opcode
    && s1_reg.dins[154 : 150] == s1_reg.ins.rd
    && s1_reg.dins[149 : 145] == s1_reg.ins.r1
    && s1_reg.dins[144 : 140] == s1_reg.ins.r2
    && s1_reg.dins[139 : 135] == s1_reg.ins.r3
    && s1_reg.dins[134 : 123] == s1_reg.ins.csr
    && s1_reg.dins[122 : 120] == s1_reg.ins.funct3
    && s1_reg.dins[119 : 115] == s1_reg.ins.funct5
    && s1_reg.dins[114 : 108] == s1_reg.ins.funct7
    && s1_reg.dins[107 : 98]  == s1_reg.ins.funct10
    && s1_reg.dins[97 : 93]   == s1_reg.ins.funct5rs2
    && s1_reg.dins[92 : 88]   == s1_reg.ins.funct5rd
    && s1_reg.dins[87 : 76]   == s1_reg.ins.iimm12
    && s1_reg.dins[75 : 64]   == s1_reg.ins.simm12
    && s1_reg.dins[63 : 51]   == s1_reg.ins.bimm13
    && s1_reg.dins[50 : 31]   == s1_reg.ins.uimm20
    && s1_reg.dins[30 : 10]   == s1_reg.ins.jimm21
    && s1_reg.dins[9 : 6]     == s1_reg.ins.pred
    && s1_reg.dins[5 : 2]     == s1_reg.ins.succ
    && s1_reg.dins[1 : 0]     == s1_reg.ins.agrl
);

helper_1:
assert property (
    $rose(gpr_state == 2'd2)
|->
    regfile[1 : 31] == '{31{almighty_cap}}
);

helper_2:
assert property (
    $rose(s1_latch)
|->
    s1_pcc.cap_pipe == next_pcc
);

inv_1:
assert property (
   (rg_enable -> cpu_state >= 4'd2)
&& (cpu_state >= 4'd2 -> gpr_state == 2'd2)
&& (cpu_state != 4'd1 -> !stageF_f_reset_reqs$EMPTY_N && stageF_f_reset_reqs$FULL_N
                      && !stageF_f_reset_rsps$EMPTY_N && stageF_f_reset_rsps$FULL_N
                      && !stageD_f_reset_reqs$EMPTY_N && stageD_f_reset_reqs$FULL_N
                      && !stageD_f_reset_rsps$EMPTY_N && stageD_f_reset_rsps$FULL_N
                      && !stage1_f_reset_reqs$EMPTY_N && stage1_f_reset_reqs$FULL_N
                      && !stage1_f_reset_rsps$EMPTY_N && stage1_f_reset_rsps$FULL_N
                      && !stage2_f_reset_reqs$EMPTY_N && stage2_f_reset_reqs$FULL_N
                      && !stage2_f_reset_rsps$EMPTY_N && stage2_f_reset_rsps$FULL_N
                      && !stage3_f_reset_reqs$EMPTY_N && stage3_f_reset_reqs$FULL_N
                      && !stage3_f_reset_rsps$EMPTY_N && stage3_f_reset_rsps$FULL_N)
&& (s1_full -> 4'd3 <= cpu_state && cpu_state <= 4'd11)
&& (s2_full -> 4'd3 <= cpu_state && cpu_state <= 4'd5)
&& (s3_full -> 4'd3 == cpu_state || cpu_state == 4'd5)
);

inv_2:
assert property (
   (cpu_state >= 4'd2 -> pipeConsistent(next_pcc) && pipeConsistent(ddc))

&& (cpu_state == 4'd6
 || cpu_state == 4'd7
 || cpu_state == 4'd8 -> pipeConsistent(csr_val1))

&& (cpu_state == 4'd4
 || cpu_state == 4'd6
 || cpu_state == 4'd7
 || cpu_state == 4'd8
 || cpu_state == 4'd9 -> pccConsistent(trap_info.epcc))

&& (gpr_state == 2'd2 -> consistent(regfile[5'd01]) && consistent(regfile[5'd02])
                      && consistent(regfile[5'd03]) && consistent(regfile[5'd04])
                      && consistent(regfile[5'd05]) && consistent(regfile[5'd06])
                      && consistent(regfile[5'd07]) && consistent(regfile[5'd08])
                      && consistent(regfile[5'd09]) && consistent(regfile[5'd10])
                      && consistent(regfile[5'd11]) && consistent(regfile[5'd12])
                      && consistent(regfile[5'd13]) && consistent(regfile[5'd14])
                      && consistent(regfile[5'd15]) && consistent(regfile[5'd16])
                      && consistent(regfile[5'd17]) && consistent(regfile[5'd18])
                      && consistent(regfile[5'd19]) && consistent(regfile[5'd20])
                      && consistent(regfile[5'd21]) && consistent(regfile[5'd22])
                      && consistent(regfile[5'd23]) && consistent(regfile[5'd24])
                      && consistent(regfile[5'd25]) && consistent(regfile[5'd26])
                      && consistent(regfile[5'd27]) && consistent(regfile[5'd28])
                      && consistent(regfile[5'd29]) && consistent(regfile[5'd30])
                      && consistent(regfile[5'd31]))
&& consistent(mtcc)
&& consistent(mtdc)
&& consistent(mscratchc)
&& consistent(mepcc)

&& (s1_latch -> pccConsistent(s1_pcc))
&& (s2_latch -> pccConsistent(s2_reg.pcc) && pipeConsistent(s2_reg.val1)
                                          && pipeConsistent(s2_reg.val2))
&& (s3_latch -> consistent(s3_reg.rd_val))
);

live_1: assert property (
    s1_latch && s1_enable && s1_reg.epoch == sd_reg.epoch
|->
    sd_reg.instr_seq == s1_reg.instr_seq + 23'd1
);

live_2: assert property (
    s1_enable
|=>
    ##[0 : 8] (
           s1_enable
        || cpu_state == 4'd10 // FENCE_I
        || cpu_state == 4'd11 // FENCE
        || cpu_state == 4'd13 // WFI
    )
);




interface prop_arith (input s2_ins, input [4 : 0] s2_rd, input retire s2r, input s3_ins, input [4 : 0] s3_rd, input retire s3r);

default clocking @(posedge CLK); endclocking

wire s2_valid =
       (!s2r.ignore_result ->
               s2_out.rd_valid
            && s2_out.rd_val == s2r.result)
    && s2_out.rd == s2_rd
    && s2_reg.op == s2r.op
    && !s2r.exc
    && (s2r.check_enable ->
               s2_reg.check_enable
            && s2_reg.check_inclusive    == s2r.check_inclusive
            && s2_reg.check_authority    == {s2r.authority, getMeta(s2r.authority)}
            && s2_reg.check_address_low  == s2r.low
            && s2_reg.check_address_high == s2r.high[64 : 0])
    && (s2r.check_exact ->
               s2_reg.check_exact_enable
            && s2_reg.check_exact_success == s2r.exact)
    && s2_reg.mem_allow_cap == s2r.mem_allow_cap;

wire s3_valid =
       s3_reg.rd_valid
    && (!s3r.ignore_result -> s3_reg.rd_val == s3r.result)
    && s3_reg.rd == s3_rd
    && !s3r.exc
    && (s3r.check_enable -> !s3r.check)
    && (s3r.check_exact -> s3r.exact);

s2:
assert property (s2_full && s2_ins |-> s2_valid);

s3:
assert property (s3_full && s3_ins |-> s3_valid);

endinterface




interface prop_branch (input cap_branch, input xlen alu_pc, input bresult alu_pcc, input s2_ins, input s2_cond);

default clocking @(posedge CLK); endclocking

wire xlen next_pc_local =
    s2_cond ? alu_pc :
    getCapOffset(s2_reg.pcc.cap_pipe.cap) + 4;

wire bresult next_pcc_local =
    s2_cond && cap_branch ? alu_pcc :
    setCapOffset(s2_reg.pcc.cap_pipe.cap, next_pc_local);

wire xlen pred =
    s2_cond && cap_branch ? alu_pcc.result.addr :
    s2_reg.pcc.base + next_pc_local; // TODO

wire branch_success =
       (next_pcc_local.flag -> next_pcc.cap == next_pcc_local.result)
    || $past(s1_reg.pred_fetch_addr) == pred;

s2: assert property (s2_enable ##1 s2_ins |-> branch_success);

endinterface




interface prop_mld (input s3_ins, input retire s1r, input [2 : 0] f3, input un);

default clocking @(posedge CLK); endclocking

wire ccap mask = {s2_reg.mem_allow_cap, ~128'b0};

sequence load;
       s2_enable
    && m_enable
    && !m_store
    && m_addr == s1r.low
    && m_f3 == f3
    && m_unsigned == un
##1
    (
        (
            !m_enable[*]
        ##1
               m_valid
            && !m_exc
            && s2_out.rd_val == (f3 == 3'b100 ? decompress(m_out & mask) : embedInt(m_out.address))
        ##1
            $stable(s2_out.rd_val)[*]
        )
    intersect
        stall_s2
    )
##1
    s3_reg.rd_val == $past(s2_out.rd_val)
##1
    $stable(s3_reg.rd_val)[*];
endsequence

s3: assert property (
       s3_full
    && s3_ins
|->
    load.triggered
);

endinterface




interface prop_mst (input s3_ins, input retire s1r, input [2 : 0] f3);

default clocking @(posedge CLK); endclocking

sequence store;
       s2_enable
    && m_enable
    && m_store
    && m_addr == s1r.low
    && m_f3 == f3
    && !m_unsigned
    && m_in == compress(f3 == 3'b100 ? s1c2 : embedInt(s1c2.addr))
##1
    (
        (
            !m_enable[*]
        ##1
               m_valid
            && !m_exc
        ##1
            1[*]
        )
    intersect
        stall_s2
    )
##[+]
    1;
endsequence

s3: assert property (
       s3_full
    && s3_ins
|->
    store.triggered
);

endinterface




// Branch
`BRA(BEQ,	1100011	, `F3(S, 000), '0, getCapOffset(s2_reg.pcc.cap_pipe.cap) + s2_reg.ins.bimm13, , V1 == V2)
`BRA(BNE,	1100011	, `F3(S, 001), '0, getCapOffset(s2_reg.pcc.cap_pipe.cap) + s2_reg.ins.bimm13, , V1 != V2)
`BRA(BLT,	1100011	, `F3(S, 100), '0, getCapOffset(s2_reg.pcc.cap_pipe.cap) + s2_reg.ins.bimm13, , V1 <  V2)
`BRA(BGE,	1100011	, `F3(S, 101), '0, getCapOffset(s2_reg.pcc.cap_pipe.cap) + s2_reg.ins.bimm13, , V1 >= V2)
`BRA(BLTU,	1100011	, `F3(S, 110), '0, getCapOffset(s2_reg.pcc.cap_pipe.cap) + s2_reg.ins.bimm13, , unsigned'(V1) <  unsigned'(V2))
`BRA(BGEU,	1100011	, `F3(S, 111), '0, getCapOffset(s2_reg.pcc.cap_pipe.cap) + s2_reg.ins.bimm13, , unsigned'(V1) >= unsigned'(V2))

// Jump
`ARI(JAL,	1101111	,  , , '{result: embedInt(getCapOffset(S.pcc.cap_pipe.cap) + 4), default: '0})
`ARI(JALR,	1100111	,  , `F3(S, 000), '{result: embedInt(getCapOffset(S.pcc.cap_pipe.cap) + 4), default: '0})
`ARI(CJALR,	`CAP	,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h0c, cJALR(C1, S.pcc.cap_pipe.cap))
`ARI(CInvoke,	`CAP	,'1, `F3(S, 000) `F7(S, 1111110) && S.ins.rd == 5'h01, cInvoke(C1, C2))

`BRA(JAL,	1101111	, , '0, getCapOffset(s2_reg.pcc.cap_pipe.cap) + s2_reg.ins.jimm21, , )
`BRA(JALR,	1100111	, `F3(S, 000), '0, (s2c1.addr + s2_reg.ins.iimm12) & ~1, , )
`BRA(CJALR,	`CAP	, `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h0c, '1, , '{result: setOType(setPointer(s2c1, s2c1.addr & ~1), '1), default: '1}, )
`BRA(CInvoke,	`CAP	, `F3(S, 000) `F7(S, 1111110) && S.ins.rd == 5'h01, '1, , '{result: setOType(setPointer(s2c1, s2c1.addr & ~1), '1), default: '1}, )

// CHERI
`ARI(AUIPCC,		0010111	,   , && S.pcc.cap_pipe.cap.flags, cAUIPCC(S.pcc.cap_pipe.cap, signed'({S.ins.uimm20, 12'b0})))

`ARI(CIncOffsetImm,	`CAP,  , `F3(S, 001), cIncOffset(C1, S.ins.iimm12))
`ARI(CSetBoundsImm,	`CAP,  , `F3(S, 010), cSetBounds(C1, unsigned'(S.ins.iimm12)))

`ARI(CSetBounds,	`CAP,  , `F3(S, 000) `F7(S, 0001000), cSetBounds(C1, V2))
`ARI(CSetBoundsExact,	`CAP,  , `F3(S, 000) `F7(S, 0001001), cSetBoundsExact(C1, V2))
`ARI(CSeal,		`CAP,  , `F3(S, 000) `F7(S, 0001011), cSeal(C1, C2))
/* `ARI(CUnseal,		`CAP,  , `F3(S, 000) `F7(S, 0001100), cUnseal(C1, C2)) */
`ARI(CAndPerm,		`CAP,  , `F3(S, 000) `F7(S, 0001101), '{result: setPerms(C1, C1.perms & {V2[18 : 15], V2[11 : 0]}), default: '0})
`ARI(CSetFlags,		`CAP,  , `F3(S, 000) `F7(S, 0001110), '{result: setFlags(C1, V2), default: '0})
`ARI(CSetOffset,	`CAP,  , `F3(S, 000) `F7(S, 0001111), cSetOffset(C1, V2))
`ARI(CSetAddr,		`CAP,  , `F3(S, 000) `F7(S, 0010000), cSetAddr(C1, V2))
`ARI(CIncOffset,	`CAP,  , `F3(S, 000) `F7(S, 0010001), cIncOffset(C1, V2))
`ARI(CToPtr,		`CAP,  , `F3(S, 000) `F7(S, 0010010), '{result: embedInt(C1.isCapability ? V1 - getCapBase(S.ins.r2 ? C2 : ddc.cap) : '0), default: '0})
`ARI(CFromPtr,		`CAP,  , `F3(S, 000) `F7(S, 0010011), cFromPtr(S.ins.r1 ? C1 : ddc.cap, V2))
`ARI(CSub,		`CAP,  , `F3(S, 000) `F7(S, 0010100), '{result: embedInt(V1 - V2), default: '0})
`ARI(CBuildCap,		`CAP,  , `F3(S, 000) `F7(S, 0011101), cBuildCap(S.ins.r1 ? C1 : ddc.cap, C2))
`ARI(CCopyType,		`CAP,  , `F3(S, 000) `F7(S, 0011110), cCopyType(C1, C2))
/* `ARI(CCSeal,		`CAP,  , `F3(S, 000) `F7(S, 0011111), cCSeal(C1, C2)) */
`ARI(CTestSubset,	`CAP,  , `F3(S, 000) `F7(S, 0100000), cTestSubset(S.ins.r1 ? C1 : ddc.cap, C2))
`ARI(CSetEqualExact,	`CAP,  , `F3(S, 000) `F7(S, 0100001), '{result: embedInt(compress(C1) == compress(C2)), default: '0})

`ARI(CGetPerm,		`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h00, '{result: embedInt({C1.perms[15 : 12], 3'b0, C1.perms[11 : 0]}), default: '0})
`ARI(CGetType,		`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h01, '{result: embedInt(hasReservedOType(C1) ? xlen'(signed'(C1.otype)) : xlen'(C1.otype)), default: '0})
`ARI(CGetBase,		`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h02, '{result: embedInt(getCapBase(C1)), default: '0})
`ARI(CGetLength,	`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h03, '{result: embedInt(getCapLength(C1)), default: '0})
`ARI(CGetTag,		`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h04, '{result: embedInt(C1.isCapability), default: '0})
`ARI(CGetSealed,	`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h05, '{result: embedInt(C1.otype != '1), default: '0})
`ARI(CGetOffset,	`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h06, '{result: embedInt(getCapOffset(C1)), default: '0})
`ARI(CGetFlags,		`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h07, '{result: embedInt(C1.flags), default: '0})
`ARI(CRRL,		`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h08, '{result: embedInt(getRepresentableLength(V1)), default: '0})
`ARI(CRAM,		`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h09, '{result: embedInt(getRepresentableAlignmentMask(V1)), default: '0})
`ARI(CMove,		`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h0a, '{result: C1, default: '0})
`ARI(CClearTag,		`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h0b, '{result: setTag(C1, '0), default: '0})
`ARI(CGetAddr,		`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h0f, '{result: embedInt(V1), default: '0})
`ARI(CSealEntry,	`CAP,  , `F3(S, 000) `F7(S, 1111111) && S.ins.r2 == 5'h11, cSealEntry(C1))

// Loads
`MLD(LB,	0000011	, `F3(S, 000), 000, '0, , )
`MLD(LH,	0000011	, `F3(S, 001), 001, '0, , )
`MLD(LW,	0000011	, `F3(S, 010), 010, '0, , )
`MLD(LD,	0000011	, `F3(S, 011), 011, '0, , )
`MLD(LC,	0001111	, `F3(S, 010), 100, '0, , )
`MLD(LBU,	0000011	, `F3(S, 100), 000, '1, , )
`MLD(LHU,	0000011	, `F3(S, 101), 001, '1, , )
`MLD(LWU,	0000011	, `F3(S, 110), 010, '1, , )

`MLD(LB_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h08, 000, '0, '1, '0)
`MLD(LH_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h09, 001, '0, '1, '0)
`MLD(LW_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h0a, 010, '0, '1, '0)
`MLD(LD_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h0b, 011, '0, '1, '0)
`MLD(LBU_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h0c, 000, '1, '1, '0)
`MLD(LHU_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h0d, 001, '1, '1, '0)
`MLD(LWU_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h0e, 010, '1, '1, '0)
`MLD(LC_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h1f, 100, '0, '1, '0)

`MLD(LB_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h00, 000, '0, '0, '0)
`MLD(LH_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h01, 001, '0, '0, '0)
`MLD(LW_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h02, 010, '0, '0, '0)
`MLD(LD_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h03, 011, '0, '0, '0)
`MLD(LBU_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h04, 000, '1, '0, '0)
`MLD(LHU_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h05, 001, '1, '0, '0)
`MLD(LWU_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h06, 010, '1, '0, '0)
`MLD(LC_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111101) && S.ins.r2 == 5'h17, 100, '0, '0, '0)

// Stores
`MST(SB,	0100011	, `F3(S, 000), 000, , )
`MST(SH,	0100011	, `F3(S, 001), 001, , )
`MST(SW,	0100011	, `F3(S, 010), 010, , )
`MST(SD,	0100011	, `F3(S, 011), 011, , )
`MST(SC,	0100011	, `F3(S, 100), 100, , )

`MST(SB_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111100) && S.ins.rd == 5'h8, 000, '1, '0)
`MST(SH_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111100) && S.ins.rd == 5'h9, 001, '1, '0)
`MST(SW_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111100) && S.ins.rd == 5'ha, 010, '1, '0)
`MST(SD_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111100) && S.ins.rd == 5'hb, 011, '1, '0)
`MST(SC_CAP,	`CAP	, `F3(S, 000) `F7(S, 1111100) && S.ins.rd == 5'hc, 100, '1, '0)

`MST(SB_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111100) && S.ins.rd == 5'h0, 000, '0, '0)
`MST(SH_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111100) && S.ins.rd == 5'h1, 001, '0, '0)
`MST(SW_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111100) && S.ins.rd == 5'h2, 010, '0, '0)
`MST(SD_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111100) && S.ins.rd == 5'h3, 011, '0, '0)
`MST(SC_DDC,	`CAP	, `F3(S, 000) `F7(S, 1111100) && S.ins.rd == 5'h4, 100, '0, '0)

endmodule
