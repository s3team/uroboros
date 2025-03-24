from arm_inst import Inst


class ArmConcreteEval(object):
    def __init_reg_dict(self):
        self.reg_value_dict = {}
        self.__data_dict = {}  # dict[int ,str]
        for reg in Inst.reg_list:
            self.reg_value_dict[reg] = -1

    def init_code_data(self):
        with open("text_section_as_data.txt", "r") as f:
            lines = f.readlines()
            for line in lines:
                line = line.rstrip("\n")
                line_arr = line.split(":")
                addr = int(line_arr[0], 16)
                self.__data_dict[addr] = line_arr[1]

    def init_got_data(self):
        got_start_addr = None
        got_size = None

        with open("sections.info", "r") as f:
            lines = f.readlines()
            for line in lines:
                line = line.rstrip("\n")
                line_arr = line.split(" ")
                section_name = line_arr[0]
                start_addr = int(line_arr[1], 16)
                _ = int(line_arr[2], 16)
                size = int(line_arr[3], 16)
                if ".got" in section_name:
                    got_start_addr = start_addr
                    got_size = size
                    break

        with open("got_split.info", "r") as f:
            lines = f.readlines()
            lines.reverse()
            data_addr = got_start_addr
            word_data_str = ""
            for line in lines:
                line = line.rstrip("\n")
                line_arr = line.split(" ")
                byte_data_str = line_arr[1].replace("0x", "")
                word_data_str = byte_data_str + word_data_str

                data_addr += 1
                if data_addr % 4 == 0:
                    word_data_str = hex(int(word_data_str, 16) - 1).replace("0x", "")
                    self.__data_dict[data_addr - 4] = word_data_str
                    word_data_str = ""

    def __init__(self):
        self.__init_reg_dict()
        self.init_code_data()
        self.init_got_data()

    def __get_pc_value(self, addr: int):
        return addr + 4

    def __eval_add(self, inst: Inst):
        # Sanity check
        src_regs = []
        for reg, _ in self.reg_value_dict.items():
            if reg in inst.src_exp:
                src_regs.append(reg)

        if reg in src_regs:
            if self.reg_value_dict[reg] == -1:
                raise ValueError(f"Register {reg} is not initialized.")

        # Evaluate the instruction
        sum = 0
        if len(inst.src_exp.split(",")) == 1:
            sum += self.reg_value_dict[inst.dst_exp]

        src_exp_arr = inst.src_exp.split(",")
        for exp in src_exp_arr:
            if "#" in exp:
                sum += int(exp.replace("#", ""))
            else:
                sum += self.reg_value_dict[exp]

        # Update the destination register
        self.reg_value_dict[inst.dst_exp] = sum

    def __eval_ldr(self, inst: Inst):
        if "[" not in inst.src_exp or "]" not in inst.src_exp:
            raise ValueError("Invalid expression for ldr instruction.")

        exp = inst.src_exp.replace("[", "").replace("]", "")
        exp_arr = exp.split(",")
        if len(exp_arr) != 2:
            raise ValueError("Invalid expression for ldr instruction.")

        src_reg = exp_arr[0]
        src_offset = None
        if "#" in exp_arr[1]:
            # objdump gives decimal numbers as immediate values
            src_offset = int(exp_arr[1].replace("#", ""))
        else:
            src_offset = self.reg_value_dict[exp_arr[1].strip()]

        src_reg_value = self.reg_value_dict[src_reg]
        data_addr = src_reg_value + src_offset
        data_addr = data_addr - (data_addr % 4)
        loaded_data = int(self.__data_dict[data_addr], 16)
        self.reg_value_dict[inst.dst_exp] = loaded_data

    def eval(self, inst: Inst):
        """
        Evaluate the instruction and update the register values.
        """
        self.reg_value_dict["pc"] = self.__get_pc_value(inst.addr)
        # inst.print()
        if inst.opcode == "add":
            self.__eval_add(inst)
        elif inst.opcode == "ldr":
            self.__eval_ldr(inst)

    def run(self, insts: list[Inst]):
        """
        Run the instruction and update the register values.
        """
        for i in insts:
            self.eval(i)
