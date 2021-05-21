# MIT License
#
# Copyright (c) 2021 Dapeng Gao
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

cd ~/FMCAD2021

# Design
analyze -verilog Verilog_RTL/mkBranch_Predictor.v
analyze -verilog Verilog_RTL/mkCPU.v
analyze -verilog Verilog_RTL/mkCSR_MIE.v
analyze -verilog Verilog_RTL/mkCSR_MIP.v
analyze -verilog Verilog_RTL/mkCSR_RegFile.v
analyze -verilog Verilog_RTL/mkGPR_RegFile.v
analyze -verilog Verilog_RTL/mkMMU_DCache.v
analyze -verilog Verilog_RTL/mkMMU_ICache.v
analyze -verilog Verilog_RTL/mkNear_Mem.v
analyze -verilog Verilog_RTL/mkSoC_Map.v

analyze -verilog src_bsc_lib_RTL/FIFO1.v
analyze -verilog src_bsc_lib_RTL/FIFO2.v
analyze -verilog src_bsc_lib_RTL/FIFO20.v
analyze -verilog src_bsc_lib_RTL/RegFile.v

elaborate -top mkCPU \
    -keep_array_for_module RegFile \
    -bbox true

clock CLK
reset ~RST_N

# Specification
analyze -repository 0 -sv12 spec_functions.sv
analyze -repository 0 -sv12 spec_stages.sv
analyze -repository 0 -sv12 spec.sv

elaborate -repository 0 -top spec

connect spec spec -repository 0 \
    \
    -connect cpu_state rg_state \
    -connect gpr_state gpr_regfile.rg_state \
    -connect csr_state csr_regfile.rg_state \
    \
    -connect next_pcc rg_next_pcc \
    -connect ddc rg_ddc \
    -connect csr_val1 rg_csr_val1 \
    -connect trap_info rg_trap_info \
    -connect regfile gpr_regfile.regfile.arr \
    -connect mtcc csr_regfile.rg_mtcc \
    -connect mtdc csr_regfile.rg_mtdc \
    -connect mscratchc csr_regfile.rg_mscratchc \
    -connect mepcc csr_regfile.rg_mepcc \
    -connect s1_pcc stage1_rg_pcc \
    \
    -connect sf_enable stageF_rg_refresh_pcc\$EN \
    -connect sd_enable stageD_rg_data\$EN \
    -connect s1_enable stage1_rg_stage_input\$EN \
    -connect s2_enable stage2_rg_stage2\$EN \
    -connect s3_enable stage3_rg_stage3\$EN \
    -connect rg_enable gpr_regfile\$EN_write_rd \
    \
    -connect sf_full stageF_rg_full \
    -connect sd_full stageD_rg_full \
    -connect s1_full stage1_rg_full \
    -connect s2_full stage2_rg_full \
    -connect s3_full stage3_rg_full \
    \
    -connect m_enable near_mem\$EN_dmem_req \
    -connect m_store near_mem\$dmem_req_op \
    -connect m_addr near_mem\$dmem_req_addr \
    -connect m_f3 near_mem\$dmem_req_f3 \
    -connect m_unsigned near_mem\$dmem_req_is_unsigned \
    -connect m_valid near_mem\$dmem_valid \
    -connect m_tag near_mem\$dmem_word128_fst \
    -connect m_word128 near_mem\$dmem_word128_snd \
    -connect m_store_value near_mem\$dmem_req_store_value \
    -connect m_exc near_mem\$dmem_exc \
    \
    -auto

# Hpcustom1
custom_engine -add -code hT3NZbhPPfqY2AbBQnsjfOxn6c+6e6yL+/e8fYueaMCoQAEA
# Ncustom2
custom_engine -add -code hT3NR7hPByFp3TrFSTLhUmMH4KWtJglTyV/c51BHEeZWamnJv767nE6PCak26bd3gf3XGN3rIRheufhDieCJQVISo+gNYUKhiUedBKGtsP/a18svAnlMJZudHxDFwK5ufsyoxsIyiAeSY2oi3tEuoDHr5gw42DFKAG3PqO3CEsl8Azzdt3pl5nGA1Ifv2H02eYsFzWb/nfp8PMb3F0krE/EgBcm8TD9Div8BAA
# AMcustom3
custom_engine -add -code hT3NE7hP/2Fz2XrVSbG0RDiSlOmcEY54dTxWvdDsI3//MxFcPzdQEJVlimlZshtLUKCpt0qXHu9EQAivsvCTP5Pi2s9EBccXnc85FrzRRX45WcXC5XpA9kyHdIYmOxUqsRrN5ntdfkwHRnO7f82Dn4pCsne09HV19Yd/o8VzV0c8nka+2MMhbNDEdLGC4SmTi8oNy00Nn40QJnMevDlPY3jIAQA

set_proofgrid_max_local_jobs 16
set_proofgrid_per_engine_max_local_jobs 16

set_engine_algorithm_selection_dir jg/models
set_engine_algorithm_selection auto

set_prove_cache_path jg/cache
# set_prove_cache save_only
set_prove_cache on

proof_structure -init SAFE -copy_assumes -copy_asserts
task -set SAFE

proof_structure -create assume_guarantee \
    -property [list {*decode *inv_1} {*helper*} {*inv_2}] \
    -imp_name {S_1 S_2 S_3}

prove -bg -task S_1 -engine_mode {M}
prove -bg -property {S_2::*helper_1} -engine_mode {Hp}
prove -bg -property {S_2::*helper_2} -engine_mode {M}
prove -bg -task S_3 -engine_mode {Hp}

# Arithmetic
proof_structure -create assume_guarantee \
    -property [list {*decode *inv*} {*_a.s2} {*_a.s3}] \
    -imp_name {A_G1 A_A1 A_A2}

prove -bg -task A_A1 -engine_mode {Hp}
prove -bg -task A_A2 -engine_mode {Hp}

# Branching
proof_structure -create assume_guarantee \
    -property [list {*decode *inv*} {*_b.*}] \
    -imp_name {J_G1 J_A1}

prove -bg -task J_A1 -engine_mode {Hp}

# Memory
proof_structure -create assume_guarantee \
    -property [list {*decode} {*_c.*}] \
    -imp_name {M_G1 M_A1}

prove -bg -task M_A1 -engine_mode {M}

# Liveness
task -create LIVE -set -copy_assumes -copy_related_covers -copy {*live*}
stopat stageF_branch_predictor

assume -name bp_fair {stageF_branch_predictor.RDY_predict_req}
assume -name int_fair { \
       !m_external_interrupt_req_set_not_clear \
    && !s_external_interrupt_req_set_not_clear \
    && !software_interrupt_req_set_not_clear \
    && !timer_interrupt_req_set_not_clear}
assume -name reset_fair {##1 !hart0_server_reset_request_put}
assume -name rvfi_fair {RDY_rvfi_dii_server_trace_report_get |-> EN_rvfi_dii_server_trace_report_get}
assume -name mem_fair {near_mem$EN_dmem_req |=> ##1 near_mem$dmem_valid}

prove -bg -asserts -property {*live*} -engine_mode {N}

prove -bg -all -covers -engine_mode {Ht}
