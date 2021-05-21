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

`define OP	0110011
`define OPI	0010011
`define OP32	0111011
`define OPI32	0011011
`define CAP	1011011

`define F3(S, FUNCT3) && S.ins.funct3 == 3'b``FUNCT3
`define F7(S, FUNCT7) && S.ins.funct7 == 7'b``FUNCT7

`define ARI(NAME, OPCODE, RD = S.ins.rd, DECODE, WRITE) \
    `define LAMBDA(S, C1, C2, V1, V2) S.ins.opcode == 7'b``OPCODE DECODE, RD, WRITE \
    prop_arith NAME``_a (`LAMBDA(s2_out, s2c1, s2c2, s2c1.addr, s2c2.addr), `LAMBDA(s3_reg, s3c1, s3c2, s3c1.addr, s3c2.addr));

`define BRA(NAME, OPCODE, DECODE, CAPBRANCH, DEST = '0, CAPDEST = '{default: '0}, COND = '1) \
    `define LAMBDA(S, V1, V2) S.ins.opcode == 7'b``OPCODE DECODE, COND \
    prop_branch NAME``_b (CAPBRANCH, DEST, CAPDEST, `LAMBDA(s2_reg, s2c1.addr, s2c2.addr));

`define MLD(NAME, OPCODE, DECODE, WIDTH, UN, CAP_OR_DDC = S.pcc.cap_pipe.cap.flags, IMM = S.ins.iimm12) \
    `ARI(NAME, OPCODE, , DECODE, cLoad(3'b``WIDTH, CAP_OR_DDC, ddc.cap, C1, IMM)) \
    `define LAMBDA1(S) S.ins.opcode == 7'b``OPCODE DECODE \
    `define LAMBDA2(S, C1, C2, V1, V2) cLoad(3'b``WIDTH, CAP_OR_DDC, ddc.cap, C1, IMM) \
    prop_mld NAME``_c (`LAMBDA1(s3_reg), `LAMBDA2(s1_reg, s1c1, s1c2, s1c1.addr, s1c2.addr), 3'b``WIDTH, UN);

`define MST(NAME, OPCODE, DECODE, WIDTH, CAP_OR_DDC = S.pcc.cap_pipe.cap.flags, IMM = S.ins.simm12) \
    `ARI(NAME, OPCODE, '0, DECODE, cStore(3'b``WIDTH, CAP_OR_DDC, ddc.cap, C1, C2, IMM)) \
    `define LAMBDA1(S) S.ins.opcode == 7'b``OPCODE DECODE \
    `define LAMBDA2(S, C1, C2, V1, V2) cStore(3'b``WIDTH, CAP_OR_DDC, ddc.cap, C1, C2, IMM) \
    prop_mst NAME``_c (`LAMBDA1(s3_reg), `LAMBDA2(s1_reg, s1c1, s1c2, s1c1.addr, s1c2.addr), 3'b``WIDTH);

interface ins(input word raw);

/*
typedef struct {
   Opcode    opcode;    [161 : 155]

   RegName   rd;        [154 : 150]
   RegName   rs1;       [149 : 145]
   RegName   rs2;       [144 : 140]
   RegName   rs3;       [139 : 135]
   CSR_Addr  csr;       [134 : 123]

   Bit #(3)  funct3;    [122 : 120]
   Bit #(5)  funct5;    [119 : 115]
   Bit #(7)  funct7;    [114 : 108]
   Bit #(10) funct10;   [107 : 98]

   Bit #(5)  funct5rs2; [97 : 93]
   Bit #(5)  funct5rd;  [92 : 88]

   Bit #(12) imm12_I;   [87 : 76]
   Bit #(12) imm12_S;   [75 : 64]
   Bit #(13) imm13_SB;  [63 : 51]
   Bit #(20) imm20_U;   [50 : 31]
   Bit #(21) imm21_UJ;  [30 : 10]

   Bit #(4)  pred;      [9 : 6]
   Bit #(4)  succ;      [5 : 2]

   Bit #(2)  aqrl;      [1 : 0]
   } Decoded_Instr
deriving (FShow, Bits);
*/

wire [6 : 0] opcode = raw[6 : 0];
wire [2 : 0] funct3 = raw[14 : 12];
wire [4 : 0] funct5 = raw[31 : 27];
wire [6 : 0] funct7 = raw[31 : 25];
wire [9 : 0] funct10 = {raw[31 : 25], raw[14 : 12]};

wire [4 : 0]
    funct5rs2 = raw[24 : 20],
    funct5rd = raw[11 : 7];

wire [4 : 0]
    rd = raw[11 : 7],
    r1 = raw[19 : 15],
    r2 = raw[24 : 20],
    r3 = raw[31 : 27];

wire signed [11 : 0]
    csr = raw[31 : 20],
    iimm12 = raw[31 : 20],
    simm12 = {raw[31 : 25], raw[11 : 7]};
wire signed [12 : 0]
    bimm13 = {raw[31], raw[7], raw[30 : 25], raw[11 : 8], 1'b0};
wire [19 : 0]
    uimm20 = raw[31 : 12];
wire signed [20 : 0]
    jimm21 = {raw[31], raw[19 : 12], raw[20], raw[30 : 21], 1'b0};

wire [3 : 0]
    pred = raw[27 : 24],
    succ = raw[23 : 20];

wire [1 : 0] agrl = raw[26 : 25];

endinterface




interface sd(input [259 : 0] rg);

/*
typedef struct {
   Addr       fetch_addr;                                                                       259 : 196
   Bool       refresh_pcc;                                                                      195
   Dii_Id     instr_seq;                                                                        194 : 172
   Epoch      epoch;              // Branch prediction epoch                                    171 : 170
   Priv_Mode  priv;               // Priv at which instr was fetched                            169 : 168
   Bool       is_i32_not_i16;     // True if a regular 32b instr, not a compressed (16b) instr  167
   Bool       exc;                // True if exc in icache access                               166
   Exc_Code   exc_code;                                                                         165 : 160
   WordXL     tval;               // Trap value; can be different from PC, with 'C' extension   159 :  96
   Instr      instr;              // Valid if no exception                                      95  :  64
   WordXL     pred_fetch_addr;    // Predicted next pc                                          63  :   0
   } Data_StageF_to_StageD
deriving (Bits);
*/

wire xlen fetch_addr = rg[259 : 196];
wire refresh_pcc = rg[195];
wire dii_id instr_seq  = rg[194 : 172];
wire [1 : 0] epoch = rg[171 : 170];
wire [1 : 0] priv = rg[169 : 168];
wire i32 = rg[167];
wire exc = rg[166];
wire [5 : 0] exc_code = rg[165 : 160];
ins ins(rg[95 : 64]);
wire xlen pred_fetch_addr = rg[63 : 0];

endinterface




interface s1(input [453 : 0] rg, input pcc pcc);

/*
typedef struct {
   Addr           fetch_addr;                                                                               453 : 390
   Bool           refresh_pcc;                                                                              389
   Dii_Id         instr_seq;                                                                                388 : 366
   Priv_Mode      priv;               // Priv at which instr was fetched                                    365 : 364
   Epoch          epoch;              // Branch prediction epoch                                            363 : 362

   Bool           is_i32_not_i16;     // True if a regular 32b instr, not a compressed (16b) instr          361

   Bool           exc;                // True if exc in icache access                                       360
   Exc_Code       exc_code;                                                                                 359 : 354
   WordXL         tval;               // Trap value; can be different from PC, with 'C' extension           353 : 290

   Instr          instr;              // Valid if no exception                                              289 : 258
   Instr          instr_or_instr_C;   // Valid if no exception; original (possibly compressed) instruction  257 : 242
   WordXL         pred_fetch_addr;    // Predicted next pc                                                  225 : 162
   Decoded_Instr  decoded_instr;                                                                            161 :   0
   } Data_StageD_to_Stage1
deriving (Bits);
*/

wire xlen fetch_addr = rg[453 : 390];
wire refresh_pcc = rg[389];
wire dii_id instr_seq  = rg[388 : 366];
wire [1 : 0] priv = rg[365 : 364];
wire [1 : 0] epoch = rg[363 : 362];

wire i32 = rg[361];

wire exc = rg[360];
wire [5 : 0] exc_code = rg[359 : 354];

ins ins(rg[289 : 258]);
wire xlen pred_fetch_addr = rg[225 : 162];

wire [161 : 0] dins = rg[161 : 0];

endinterface




typedef struct packed {

/*
typedef struct {
    Bit#(ILEN)  instr;
    // From decode
    Bit#(5)     rs1_addr;
    Bit#(5)     rs2_addr;
    Bit#(XLEN)  rs1_data;
    Bit#(XLEN)  rs2_data;
    Bit#(XLEN)  pc_rdata;
    // TODO: Exceptions?
    Bit#(XLEN)  pc_wdata;
    // TODO: Needs 0'ing when unused?
    Bit#(MEMWIDTH)  mem_wdata;

    // From ALU:
    Bit#(5)     rd_addr;
    // Might be killed by memory OPs.
    Bool        rd_alu;
    Bit#(XLEN)  rd_wdata_alu;

    Bit#(XLEN)  mem_addr;

} Data_RVFI_Stage1 deriving (Bits, Eq);
*/

    logic [31 : 0] instr;
    logic [4 : 0] rs1_addr, rs2_addr;
    xlen rs1_data, rs2_data, pc_rdata, pc_wdata;
    xlen mem_wdata;
    logic [4 : 0] rd_addr;
    logic rd_alu;
    xlen rd_wdata_alu;
    xlen mem_addr;

} rvfi_s1;

interface s2(input [1607 : 0] rg);

/*
typedef struct {
   Priv_Mode  priv;                                                             1607:1606
   PCC_T      pcc;                                                              1605:1380
   Instr      instr;             // For debugging. Just funct3, funct7 are      1379:1348
                                 // enough for functionality.
   Dii_Id     instr_seq;                                                        1347:1325
   Op_Stage2  op_stage2;                                                        1324:1323
   RegName    rd;                                                               1322:1318
   Addr       addr;     // Branch, jump: newPC                                  1317:1254
                        // Mem ops and AMOs: mem addr

   Pipeline_Val#(CapPipe) val1;  // OP_Stage2_ALU: rd_val                       1253:1092
                       // OP_Stage2_M and OP_Stage2_FD: arg1

   Pipeline_Val#(CapPipe) val2;  // OP_Stage2_ST: store-val;                    1091: 930
                       // OP_Stage2_M and OP_Stage2_FD: arg2

   WordXL     val1_fast;                                                        929 : 866
   WordXL     val2_fast;                                                        865 : 802

   // Bounds check: if check_enable, will test
   // address_low >= authority.base && address_high <? authority.top (?: strictness determined by check_inclusive value TODO)
   // TODO behaviour if address_low > address_high?
   // Does not check that authority is tagged, so only generates
   // Bounds exceptions
   CapPipe    check_authority;                                                  801 : 640
   Bit#(6)    check_authority_idx;                                              639 : 634
   Bit#(XLEN)     check_address_low;                                            633 : 570
   Bit#(TAdd#(XLEN,1))     check_address_high;                                  569 : 505
   Bool       check_enable;                                                     504
   Bool check_inclusive;                                                        503
   Bool check_exact_enable;                                                     502
   Bool check_exact_success;                                                    501

   Bool       mem_allow_cap;                                                    500

   Bit#(3)    mem_width_code;                                                   499 : 497
   Bool       mem_unsigned;                                                     496
   Data_RVFI_Stage1 info_RVFI_s1;                                               495 :   0

   } Data_Stage1_to_Stage2
deriving (Bits);
*/

wire [1 : 0] priv = rg[1607 : 1606];
wire pcc pcc = rg[1605 : 1380];
ins ins(rg[1379 : 1348]);
wire dii_id instr_seq  = rg[1347 : 1325];

wire [1 : 0] op = rg[1324 : 1323];
wire [4 : 0] rd = rg[1322 : 1318];

wire xlen addr = rg[1317 : 1254];
wire cap_pipe
    val1 = rg[1253 : 1092],
    val2 = rg[1091 : 930];

wire xlen
    val1_fast = rg[929 : 866],
    val2_fast = rg[865 : 802];

wire cap_pipe check_authority = rg[801 : 640];
wire xlen check_address_low = rg[633 : 570];
wire [64 : 0] check_address_high = rg[569 : 505];

wire
    check_enable = rg[504],
    check_inclusive = rg[503],
    check_exact_enable = rg[502],
    check_exact_success = rg[501];

wire mem_allow_cap = rg[500];

wire rvfi_s1 info_rvfi_s1 = rg[495 : 0];

endinterface




typedef struct packed {

/*
typedef struct {
    Data_RVFI_Stage1    stage1;
    // Hard to know what was written as SC pretends to write "0" on failure
    // instead of actual untouched value. So, indicate wmask = 0 perhaps?

    Bit#(TDiv#(MEMWIDTH,8))       mem_rmask;
    Bit#(TDiv#(MEMWIDTH,8))       mem_wmask;

}   Data_RVFI_Stage2 deriving (Bits);
*/

    rvfi_s1 stage1;
    logic [7 : 0] mem_rmask, mem_wmask;

} rvfi_s2;

interface s3(input [952 : 0] rg);

/*
typedef struct {
   PCC_T     pcc;           // For debugging only   952 : 727
   Instr     instr;         // For debugging only   726 : 695
   Dii_Id    instr_seq;                             694 : 672
   Priv_Mode priv;                                  671 : 670

   Bool      rd_valid;                              669
   RegName   rd;                                    668 : 664
   Pipeline_Val#(CapReg) rd_val;                    663 : 512
   Data_RVFI_Stage2 info_RVFI_s2;                   511 :   0
   } Data_Stage2_to_Stage3
deriving (Bits);
*/

wire pcc pcc = rg[952 : 727];
ins ins(rg[726 : 695]);
wire dii_id instr_seq  = rg[694 : 672];
wire [1 : 0] priv = rg[671 : 670];

wire rd_valid = rg[669];
wire [4 : 0] rd = rg[668 : 664];
wire cap rd_val = rg[663 : 512];

wire rvfi_s2 info_rvfi_s2 = rg[511 : 0];

endinterface
