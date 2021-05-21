//
// Generated by Bluespec Compiler (build 7d25cde)
//
//
// Ports:
// Name                         I/O  size props
// RDY_server_reset_request_put   O     1 reg
// RDY_server_reset_response_get  O     1
// read_rs1                       O   162
// read_rs1_port2                 O   162
// read_rs2                       O   162
// CLK                            I     1 clock
// RST_N                          I     1 reset
// read_rs1_rs1                   I     5
// read_rs1_port2_rs1             I     5
// read_rs2_rs2                   I     5
// write_rd_rd                    I     5
// write_rd_rd_val                I   152
// EN_server_reset_request_put    I     1
// EN_server_reset_response_get   I     1
// EN_write_rd                    I     1
//
// Combinational paths from inputs to outputs:
//   read_rs1_rs1 -> read_rs1
//   read_rs1_port2_rs1 -> read_rs1_port2
//   read_rs2_rs2 -> read_rs2
//
//

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module mkGPR_RegFile(CLK,
		     RST_N,

		     EN_server_reset_request_put,
		     RDY_server_reset_request_put,

		     EN_server_reset_response_get,
		     RDY_server_reset_response_get,

		     read_rs1_rs1,
		     read_rs1,

		     read_rs1_port2_rs1,
		     read_rs1_port2,

		     read_rs2_rs2,
		     read_rs2,

		     write_rd_rd,
		     write_rd_rd_val,
		     EN_write_rd);
  input  CLK;
  input  RST_N;

  // action method server_reset_request_put
  input  EN_server_reset_request_put;
  output RDY_server_reset_request_put;

  // action method server_reset_response_get
  input  EN_server_reset_response_get;
  output RDY_server_reset_response_get;

  // value method read_rs1
  input  [4 : 0] read_rs1_rs1;
  output [161 : 0] read_rs1;

  // value method read_rs1_port2
  input  [4 : 0] read_rs1_port2_rs1;
  output [161 : 0] read_rs1_port2;

  // value method read_rs2
  input  [4 : 0] read_rs2_rs2;
  output [161 : 0] read_rs2;

  // action method write_rd
  input  [4 : 0] write_rd_rd;
  input  [151 : 0] write_rd_rd_val;
  input  EN_write_rd;

  // signals for module outputs
  wire [161 : 0] read_rs1, read_rs1_port2, read_rs2;
  wire RDY_server_reset_request_put, RDY_server_reset_response_get;

  // register rg_j
  reg [4 : 0] rg_j;
  wire [4 : 0] rg_j$D_IN;
  wire rg_j$EN;

  // register rg_state
  reg [1 : 0] rg_state;
  reg [1 : 0] rg_state$D_IN;
  wire rg_state$EN;

  // ports of submodule f_reset_rsps
  wire f_reset_rsps$CLR,
       f_reset_rsps$DEQ,
       f_reset_rsps$EMPTY_N,
       f_reset_rsps$ENQ,
       f_reset_rsps$FULL_N;

  // ports of submodule regfile
  wire [151 : 0] regfile$D_IN,
		 regfile$D_OUT_1,
		 regfile$D_OUT_2,
		 regfile$D_OUT_3;
  wire [4 : 0] regfile$ADDR_1,
	       regfile$ADDR_2,
	       regfile$ADDR_3,
	       regfile$ADDR_4,
	       regfile$ADDR_5,
	       regfile$ADDR_IN;
  wire regfile$WE;

  // rule scheduling signals
  wire CAN_FIRE_RL_rl_reset_loop,
       CAN_FIRE_RL_rl_reset_start,
       CAN_FIRE_server_reset_request_put,
       CAN_FIRE_server_reset_response_get,
       CAN_FIRE_write_rd,
       WILL_FIRE_RL_rl_reset_loop,
       WILL_FIRE_RL_rl_reset_start,
       WILL_FIRE_server_reset_request_put,
       WILL_FIRE_server_reset_response_get,
       WILL_FIRE_write_rd;

  // inputs to muxes for submodule ports
  wire [4 : 0] MUX_rg_j$write_1__VAL_1;
  wire MUX_regfile$upd_1__SEL_1, MUX_rg_state$write_1__SEL_2;

  // remaining internal signals
  wire [63 : 0] thin_address__h1007, thin_address__h1245, thin_address__h712;
  wire [35 : 0] read_rs1_port2_rs1_EQ_0_9_OR_regfile_sub_read__ETC___d147,
		read_rs1_rs1_EQ_0_OR_regfile_sub_read_rs1_rs1__ETC___d67,
		read_rs2_rs2_EQ_0_69_OR_regfile_sub_read_rs2_r_ETC___d227;
  wire [17 : 0] thin_otype__h1012, thin_otype__h1250, thin_otype__h717;
  wire [13 : 0] thin_addrBits__h1008,
		thin_addrBits__h1246,
		thin_addrBits__h713,
		x__h1146,
		x__h1149,
		x__h1384,
		x__h1387,
		x__h905,
		x__h908;
  wire [4 : 0] IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d167,
	       IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d87,
	       IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d247;
  wire [3 : 0] thin_perms_soft__h1027,
	       thin_perms_soft__h1265,
	       thin_perms_soft__h736;
  wire [2 : 0] repBound__h1170, repBound__h1408, repBound__h929;
  wire [1 : 0] thin_reserved__h1011,
	       thin_reserved__h1249,
	       thin_reserved__h716;
  wire IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d155,
       IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d157,
       IF_read_rs1_port2_rs1_EQ_0_9_THEN_4096_ELSE_re_ETC___d154,
       IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d75,
       IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d77,
       IF_read_rs1_rs1_EQ_0_THEN_4096_ELSE_regfile_su_ETC___d74,
       IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d235,
       IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d237,
       IF_read_rs2_rs2_EQ_0_69_THEN_4096_ELSE_regfile_ETC___d234,
       thin_flags__h1010,
       thin_flags__h1248,
       thin_flags__h715;

  // action method server_reset_request_put
  assign RDY_server_reset_request_put = f_reset_rsps$FULL_N ;
  assign CAN_FIRE_server_reset_request_put = f_reset_rsps$FULL_N ;
  assign WILL_FIRE_server_reset_request_put = EN_server_reset_request_put ;

  // action method server_reset_response_get
  assign RDY_server_reset_response_get =
	     rg_state == 2'd2 && f_reset_rsps$EMPTY_N ;
  assign CAN_FIRE_server_reset_response_get =
	     rg_state == 2'd2 && f_reset_rsps$EMPTY_N ;
  assign WILL_FIRE_server_reset_response_get = EN_server_reset_response_get ;

  // value method read_rs1
  assign read_rs1 =
	     { read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[151],
	       thin_address__h712,
	       thin_addrBits__h713,
	       thin_perms_soft__h736,
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[68],
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[67],
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[66],
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[65],
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[64],
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[63],
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[62],
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[61],
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[60],
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[59],
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[58],
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[57],
	       thin_flags__h715,
	       thin_reserved__h716,
	       thin_otype__h717,
	       read_rs1_rs1_EQ_0_OR_regfile_sub_read_rs1_rs1__ETC___d67,
	       repBound__h929,
	       IF_read_rs1_rs1_EQ_0_THEN_4096_ELSE_regfile_su_ETC___d74,
	       IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d75,
	       IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d87 } ;

  // value method read_rs1_port2
  assign read_rs1_port2 =
	     { read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[151],
	       thin_address__h1007,
	       thin_addrBits__h1008,
	       thin_perms_soft__h1027,
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[68],
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[67],
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[66],
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[65],
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[64],
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[63],
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[62],
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[61],
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[60],
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[59],
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[58],
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[57],
	       thin_flags__h1010,
	       thin_reserved__h1011,
	       thin_otype__h1012,
	       read_rs1_port2_rs1_EQ_0_9_OR_regfile_sub_read__ETC___d147,
	       repBound__h1170,
	       IF_read_rs1_port2_rs1_EQ_0_9_THEN_4096_ELSE_re_ETC___d154,
	       IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d155,
	       IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d167 } ;

  // value method read_rs2
  assign read_rs2 =
	     { read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[151],
	       thin_address__h1245,
	       thin_addrBits__h1246,
	       thin_perms_soft__h1265,
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[68],
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[67],
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[66],
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[65],
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[64],
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[63],
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[62],
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[61],
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[60],
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[59],
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[58],
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[57],
	       thin_flags__h1248,
	       thin_reserved__h1249,
	       thin_otype__h1250,
	       read_rs2_rs2_EQ_0_69_OR_regfile_sub_read_rs2_r_ETC___d227,
	       repBound__h1408,
	       IF_read_rs2_rs2_EQ_0_69_THEN_4096_ELSE_regfile_ETC___d234,
	       IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d235,
	       IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d247 } ;

  // action method write_rd
  assign CAN_FIRE_write_rd = 1'd1 ;
  assign WILL_FIRE_write_rd = EN_write_rd ;

  // submodule f_reset_rsps
  FIFO20 #(.guarded(1'd1)) f_reset_rsps(.RST(RST_N),
					.CLK(CLK),
					.ENQ(f_reset_rsps$ENQ),
					.DEQ(f_reset_rsps$DEQ),
					.CLR(f_reset_rsps$CLR),
					.FULL_N(f_reset_rsps$FULL_N),
					.EMPTY_N(f_reset_rsps$EMPTY_N));

  // submodule regfile
  RegFile #(.addr_width(32'd5),
	    .data_width(32'd152),
	    .lo(5'h0),
	    .hi(5'd31)) regfile(.CLK(CLK),
				.ADDR_1(regfile$ADDR_1),
				.ADDR_2(regfile$ADDR_2),
				.ADDR_3(regfile$ADDR_3),
				.ADDR_4(regfile$ADDR_4),
				.ADDR_5(regfile$ADDR_5),
				.ADDR_IN(regfile$ADDR_IN),
				.D_IN(regfile$D_IN),
				.WE(regfile$WE),
				.D_OUT_1(regfile$D_OUT_1),
				.D_OUT_2(regfile$D_OUT_2),
				.D_OUT_3(regfile$D_OUT_3),
				.D_OUT_4(),
				.D_OUT_5());

  // rule RL_rl_reset_start
  assign CAN_FIRE_RL_rl_reset_start = rg_state == 2'd0 ;
  assign WILL_FIRE_RL_rl_reset_start = CAN_FIRE_RL_rl_reset_start ;

  // rule RL_rl_reset_loop
  assign CAN_FIRE_RL_rl_reset_loop = rg_state == 2'd1 ;
  assign WILL_FIRE_RL_rl_reset_loop =
	     CAN_FIRE_RL_rl_reset_loop && !EN_write_rd ;

  // inputs to muxes for submodule ports
  assign MUX_regfile$upd_1__SEL_1 = EN_write_rd && write_rd_rd != 5'd0 ;
  assign MUX_rg_state$write_1__SEL_2 =
	     WILL_FIRE_RL_rl_reset_loop && rg_j == 5'd31 ;
  assign MUX_rg_j$write_1__VAL_1 = rg_j + 5'd1 ;

  // register rg_j
  assign rg_j$D_IN =
	     WILL_FIRE_RL_rl_reset_loop ? MUX_rg_j$write_1__VAL_1 : 5'd1 ;
  assign rg_j$EN = WILL_FIRE_RL_rl_reset_loop || WILL_FIRE_RL_rl_reset_start ;

  // register rg_state
  always@(EN_server_reset_request_put or
	  MUX_rg_state$write_1__SEL_2 or WILL_FIRE_RL_rl_reset_start)
  case (1'b1)
    EN_server_reset_request_put: rg_state$D_IN = 2'd0;
    MUX_rg_state$write_1__SEL_2: rg_state$D_IN = 2'd2;
    WILL_FIRE_RL_rl_reset_start: rg_state$D_IN = 2'd1;
    default: rg_state$D_IN = 2'b10 /* unspecified value */ ;
  endcase
  assign rg_state$EN =
	     WILL_FIRE_RL_rl_reset_loop && rg_j == 5'd31 ||
	     EN_server_reset_request_put ||
	     WILL_FIRE_RL_rl_reset_start ;

  // submodule f_reset_rsps
  assign f_reset_rsps$ENQ = EN_server_reset_request_put ;
  assign f_reset_rsps$DEQ = EN_server_reset_response_get ;
  assign f_reset_rsps$CLR = 1'b0 ;

  // submodule regfile
  assign regfile$ADDR_1 = read_rs2_rs2 ;
  assign regfile$ADDR_2 = read_rs1_port2_rs1 ;
  assign regfile$ADDR_3 = read_rs1_rs1 ;
  assign regfile$ADDR_4 = 5'h0 ;
  assign regfile$ADDR_5 = 5'h0 ;
  assign regfile$ADDR_IN = MUX_regfile$upd_1__SEL_1 ? write_rd_rd : rg_j ;
  assign regfile$D_IN =
	     MUX_regfile$upd_1__SEL_1 ?
	       write_rd_rd_val :
	       152'h80000000000000000001FFFE3FFFFE88000000 ;
  assign regfile$WE =
	     EN_write_rd && write_rd_rd != 5'd0 ||
	     WILL_FIRE_RL_rl_reset_loop ;

  // remaining internal signals
  assign IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d155 =
	     x__h1149[13:11] < repBound__h1170 ;
  assign IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d157 =
	     thin_addrBits__h1008[13:11] < repBound__h1170 ;
  assign IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d167 =
	     { IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d157,
	       (IF_read_rs1_port2_rs1_EQ_0_9_THEN_4096_ELSE_re_ETC___d154 ==
		IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d157) ?
		 2'd0 :
		 ((IF_read_rs1_port2_rs1_EQ_0_9_THEN_4096_ELSE_re_ETC___d154 &&
		   !IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d157) ?
		    2'd1 :
		    2'd3),
	       (IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d155 ==
		IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d157) ?
		 2'd0 :
		 ((IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d155 &&
		   !IF_read_rs1_port2_rs1_EQ_0_9_THEN_0_ELSE_regfi_ETC___d157) ?
		    2'd1 :
		    2'd3) } ;
  assign IF_read_rs1_port2_rs1_EQ_0_9_THEN_4096_ELSE_re_ETC___d154 =
	     x__h1146[13:11] < repBound__h1170 ;
  assign IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d75 =
	     x__h908[13:11] < repBound__h929 ;
  assign IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d77 =
	     thin_addrBits__h713[13:11] < repBound__h929 ;
  assign IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d87 =
	     { IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d77,
	       (IF_read_rs1_rs1_EQ_0_THEN_4096_ELSE_regfile_su_ETC___d74 ==
		IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d77) ?
		 2'd0 :
		 ((IF_read_rs1_rs1_EQ_0_THEN_4096_ELSE_regfile_su_ETC___d74 &&
		   !IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d77) ?
		    2'd1 :
		    2'd3),
	       (IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d75 ==
		IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d77) ?
		 2'd0 :
		 ((IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d75 &&
		   !IF_read_rs1_rs1_EQ_0_THEN_0_ELSE_regfile_sub_r_ETC___d77) ?
		    2'd1 :
		    2'd3) } ;
  assign IF_read_rs1_rs1_EQ_0_THEN_4096_ELSE_regfile_su_ETC___d74 =
	     x__h905[13:11] < repBound__h929 ;
  assign IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d235 =
	     x__h1387[13:11] < repBound__h1408 ;
  assign IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d237 =
	     thin_addrBits__h1246[13:11] < repBound__h1408 ;
  assign IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d247 =
	     { IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d237,
	       (IF_read_rs2_rs2_EQ_0_69_THEN_4096_ELSE_regfile_ETC___d234 ==
		IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d237) ?
		 2'd0 :
		 ((IF_read_rs2_rs2_EQ_0_69_THEN_4096_ELSE_regfile_ETC___d234 &&
		   !IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d237) ?
		    2'd1 :
		    2'd3),
	       (IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d235 ==
		IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d237) ?
		 2'd0 :
		 ((IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d235 &&
		   !IF_read_rs2_rs2_EQ_0_69_THEN_0_ELSE_regfile_su_ETC___d237) ?
		    2'd1 :
		    2'd3) } ;
  assign IF_read_rs2_rs2_EQ_0_69_THEN_4096_ELSE_regfile_ETC___d234 =
	     x__h1384[13:11] < repBound__h1408 ;
  assign read_rs1_port2_rs1_EQ_0_9_OR_regfile_sub_read__ETC___d147 =
	     { read_rs1_port2_rs1 == 5'd0 || regfile$D_OUT_2[35],
	       (read_rs1_port2_rs1 == 5'd0) ? 6'd52 : regfile$D_OUT_2[34:29],
	       x__h1146,
	       x__h1149,
	       read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[0] } ;
  assign read_rs1_rs1_EQ_0_OR_regfile_sub_read_rs1_rs1__ETC___d67 =
	     { read_rs1_rs1 == 5'd0 || regfile$D_OUT_3[35],
	       (read_rs1_rs1 == 5'd0) ? 6'd52 : regfile$D_OUT_3[34:29],
	       x__h905,
	       x__h908,
	       read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[0] } ;
  assign read_rs2_rs2_EQ_0_69_OR_regfile_sub_read_rs2_r_ETC___d227 =
	     { read_rs2_rs2 == 5'd0 || regfile$D_OUT_1[35],
	       (read_rs2_rs2 == 5'd0) ? 6'd52 : regfile$D_OUT_1[34:29],
	       x__h1384,
	       x__h1387,
	       read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[0] } ;
  assign repBound__h1170 = x__h1149[13:11] - 3'b001 ;
  assign repBound__h1408 = x__h1387[13:11] - 3'b001 ;
  assign repBound__h929 = x__h908[13:11] - 3'b001 ;
  assign thin_addrBits__h1008 =
	     (read_rs1_port2_rs1 == 5'd0) ? 14'd0 : regfile$D_OUT_2[86:73] ;
  assign thin_addrBits__h1246 =
	     (read_rs2_rs2 == 5'd0) ? 14'd0 : regfile$D_OUT_1[86:73] ;
  assign thin_addrBits__h713 =
	     (read_rs1_rs1 == 5'd0) ? 14'd0 : regfile$D_OUT_3[86:73] ;
  assign thin_address__h1007 =
	     (read_rs1_port2_rs1 == 5'd0) ? 64'd0 : regfile$D_OUT_2[150:87] ;
  assign thin_address__h1245 =
	     (read_rs2_rs2 == 5'd0) ? 64'd0 : regfile$D_OUT_1[150:87] ;
  assign thin_address__h712 =
	     (read_rs1_rs1 == 5'd0) ? 64'd0 : regfile$D_OUT_3[150:87] ;
  assign thin_flags__h1010 =
	     read_rs1_port2_rs1 != 5'd0 && regfile$D_OUT_2[56] ;
  assign thin_flags__h1248 = read_rs2_rs2 != 5'd0 && regfile$D_OUT_1[56] ;
  assign thin_flags__h715 = read_rs1_rs1 != 5'd0 && regfile$D_OUT_3[56] ;
  assign thin_otype__h1012 =
	     (read_rs1_port2_rs1 == 5'd0) ?
	       18'd262143 :
	       regfile$D_OUT_2[53:36] ;
  assign thin_otype__h1250 =
	     (read_rs2_rs2 == 5'd0) ? 18'd262143 : regfile$D_OUT_1[53:36] ;
  assign thin_otype__h717 =
	     (read_rs1_rs1 == 5'd0) ? 18'd262143 : regfile$D_OUT_3[53:36] ;
  assign thin_perms_soft__h1027 =
	     (read_rs1_port2_rs1 == 5'd0) ? 4'd0 : regfile$D_OUT_2[72:69] ;
  assign thin_perms_soft__h1265 =
	     (read_rs2_rs2 == 5'd0) ? 4'd0 : regfile$D_OUT_1[72:69] ;
  assign thin_perms_soft__h736 =
	     (read_rs1_rs1 == 5'd0) ? 4'd0 : regfile$D_OUT_3[72:69] ;
  assign thin_reserved__h1011 =
	     (read_rs1_port2_rs1 == 5'd0) ? 2'd0 : regfile$D_OUT_2[55:54] ;
  assign thin_reserved__h1249 =
	     (read_rs2_rs2 == 5'd0) ? 2'd0 : regfile$D_OUT_1[55:54] ;
  assign thin_reserved__h716 =
	     (read_rs1_rs1 == 5'd0) ? 2'd0 : regfile$D_OUT_3[55:54] ;
  assign x__h1146 =
	     (read_rs1_port2_rs1 == 5'd0) ?
	       14'd4096 :
	       regfile$D_OUT_2[28:15] ;
  assign x__h1149 =
	     (read_rs1_port2_rs1 == 5'd0) ? 14'd0 : regfile$D_OUT_2[14:1] ;
  assign x__h1384 =
	     (read_rs2_rs2 == 5'd0) ? 14'd4096 : regfile$D_OUT_1[28:15] ;
  assign x__h1387 = (read_rs2_rs2 == 5'd0) ? 14'd0 : regfile$D_OUT_1[14:1] ;
  assign x__h905 =
	     (read_rs1_rs1 == 5'd0) ? 14'd4096 : regfile$D_OUT_3[28:15] ;
  assign x__h908 = (read_rs1_rs1 == 5'd0) ? 14'd0 : regfile$D_OUT_3[14:1] ;

  // handling of inlined registers

  always@(posedge CLK)
  begin
    if (RST_N == `BSV_RESET_VALUE)
      begin
        rg_state <= `BSV_ASSIGNMENT_DELAY 2'd0;
      end
    else
      begin
        if (rg_state$EN) rg_state <= `BSV_ASSIGNMENT_DELAY rg_state$D_IN;
      end
    if (rg_j$EN) rg_j <= `BSV_ASSIGNMENT_DELAY rg_j$D_IN;
  end

  // synopsys translate_off
  `ifdef BSV_NO_INITIAL_BLOCKS
  `else // not BSV_NO_INITIAL_BLOCKS
  initial
  begin
    rg_j = 5'h0A;
    rg_state = 2'h2;
  end
  `endif // BSV_NO_INITIAL_BLOCKS
  // synopsys translate_on
endmodule  // mkGPR_RegFile

