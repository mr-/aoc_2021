import re


def read_file(filename):
    file1 = open(filename, 'r')

    content = file1.read()

    spassports = content.split("\n\n")
    passports = []
    for passport in spassports:
        entries = re.split(r"[\n ]", passport)
        p = {}
        for entry in entries:
            if len(entry) > 0:
                k, v = entry.split(':')
                p[k] = v
        passports.append(p)
    return passports


mandatory_fields = set(["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"])


def isbetween(l, h, d):
    return l <= d and h >= d


def validate_height(s):
    m = re.fullmatch(r"(\d+)cm", s)
    if m:
        return isbetween(150, 193, int(m.group(1)))

    m = re.fullmatch(r"(\d+)in", s)
    if m:
        return isbetween(59, 76, int(m.group(1)))

    return False


validations = {
    "byr": lambda s: s.isdigit() and isbetween(1920, 2002, int(s)),
    "iyr": lambda s: s.isdigit() and isbetween(2010, 2020, int(s)),
    "eyr": lambda s: s.isdigit() and isbetween(2020, 2030, int(s)),
    "hcl": lambda s: re.fullmatch(r"#[0123456789abcdef]{6}", s) is not None,
    "ecl": lambda s: s in set(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]),
    "hgt": validate_height,
    "pid": lambda s: len(s) == 9 and s.isdigit(),
    "cid": lambda s: True

}

passports = read_file("sample2.txt")
passports = read_file("input.txt")


def validate_passport(passport):
    if not mandatory_fields.issubset(passport.keys()):
        # print("Missing fields..")
        return False
    for key in passport.keys():
        if not validations[key](passport[key]):
            # print(f"Failed validation {key}:{passport[key]}")
            return False
    return True


valids = sum([1 for passport in passports if validate_passport(passport)])
print(f"found {valids} valid passports")
