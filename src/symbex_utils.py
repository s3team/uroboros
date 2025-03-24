import angr


def symbex(
    filename: str, start_addr: int, end_addr: int, is_thumb: bool
) -> angr.SimState:
    """
    Run symbolic execution along the instructions from start_addr to end_addr.
    Then, return the first active state.

    Args:
        filename (str): Filename of the binary
        start_addr (int): Start address.
        end_addr (int): End address. The instruction at this address is NOT executed.

    Returns:
        _type_: _description_
    """
    proj = angr.Project(filename)
    init_state = proj.factory.blank_state(
        addr=start_addr,
        add_options={
            angr.options.SYMBOL_FILL_UNCONSTRAINED_REGISTERS,
            angr.options.SYMBOL_FILL_UNCONSTRAINED_MEMORY,
        },
    )

    simgr = proj.factory.simulation_manager(init_state)
    while simgr.active:
        state = simgr.active[0]
        block = state.block()
        print(block.disassembly.insns[0])

        simgr.step(num_inst=1, thumb=is_thumb)
        # print the instruction
        # print the registers of the first active state
        # print in hex format
        sl = simgr.active[0].regs.sl.concrete_value
        r3 = simgr.active[0].regs.r3.concrete_value
        r4 = simgr.active[0].regs.r4.concrete_value
        pc = simgr.active[0].regs.pc.concrete_value
        # check if sl is int
        if isinstance(sl, int):
            sl = hex(sl)
        if isinstance(r3, int):
            r3 = hex(r3)
        if isinstance(r4, int):
            r4 = hex(r4)
        if isinstance(pc, int):
            pc = hex(pc)

        print("sl:", sl)
        print("r3:", r3)
        print("r4:", r4)
        print("pc:", pc)
        print("")

        if simgr.active[0].addr >= (end_addr + is_thumb):
            print("r3 mem:", simgr.active[0].mem[r3])
            break

    if not simgr.active:
        raise Exception("No active state")

    return simgr


if __name__ == "__main__":
    import sys

    if len(sys.argv) != 5:
        print("Usage: python symbex.py <filename> <start_addr> <end_addr>")
        sys.exit(1)

    filename = sys.argv[1]
    start_addr = int(sys.argv[2], 16)
    end_addr = int(sys.argv[3], 16)
    is_thumb = 0x1 if sys.argv[4] == "thumb" else 0x0

    simgr = symbex(filename, start_addr, end_addr, is_thumb)
