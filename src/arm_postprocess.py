def adjust_alignment(filename):
    new_content = []
    with open(filename, "r") as f:
        lines = f.readlines()
        for line in lines:
            new_content.append(line)
            if "bhi" in line:
                # align "bhi" in case it is related to swtich statement
                new_content.append(".align 4\n")

    with open(filename, "w") as f:
        f.writelines(new_content)


def main(argv):
    if len(argv) != 2:
        print("Usage: python arm_postprocess.py <path_to_binary>")
        sys.exit(1)

    binary_path = argv[1]
    adjust_alignment(binary_path)


if __name__ == "__main__":
    import sys

    main(sys.argv)
