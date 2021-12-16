import functools
from functools import reduce
import networkx as nx
import math

puzzle_input = "A20D790042F1274011955491808B802F1C60B20030327AF2CC248AA800E7CDD726F3D78F4966F571A300BA54D668E2519249265160803EA9DE562A1801204ACE53C954ACE53C94C659BDF318FD1366EF44D96EB11005FB39154E0068A7C3A6B379646C80348A0055E6642B332109B8D6F0F12980452C9D322B28012EC72D51B300426CF70017996DE6C2B2C70C01A04B67B9F9EC8DAFE679D0992A80380104065FA8012805BD380120051E380146006380142004A00E920034C0801CA007B0099420053007144016E28018800CCC8CBB5FE79A3D91E1DC9FB151A1006CC0188970D6109803B1D61344320042615C198C2A014C589D00943096B3CCC081009173D015B004C401C8E10421E8002110BA18C193004A52257E0094BCE1ABB94C2C9005112DFAA5E80292B405927020106BC01494DFA6E329BF4DD273B69E233DB04C435BEF7A0CC00CFCDF31DC6AD20A3002A498CC01D00042229479890200E4438A91700010F88F0EA251802D33FE976802538EF38E2401B84CA05004833529CD2A5BD9DDAC566009CC33E8024200CC528E71F40010A8DF0C61D8002B5076719A5D418034891895CFD320730F739A119CB2EA0072D25E870EA465E189FDC1126AF4B91100A03600A0803713E2FC7D00043A25C3B8A12F89D2E6440242489A7802400086C788FB09C0010C8BB132309005A1400D2CBE7E7F2F9F9F4BB83803B25286DFE628E129EBCB7483C8802F3D0A2542E3004AC0169BD944AFF263361F1B48010496089807100BA54A66675769B1787D230C621EF8B9007893F058A009AE4ED7A5BBDBE05262CEC0002FC7C20082622E0020D0D66A2D04021D5003ED3D396E19A1149054FCA3586BD00020129B0037300042E0CC1184C000874368F70A251D840239798AC8DC9A56F7C6C0E0728015294D9290030B226938A928D0"

def parse_header(bits):
    version = bits[:3]
    type_id = bits[3:6]

    return ({"version": int(version, 2), "type": int(type_id,2)}, bits[6:])

def parse_operator_header(bits):
    length_type_id = bits[0]

    if length_type_id == '0':
        subpacket_length = int(bits[1:16], 2)
        return ({"length_type": length_type_id, "subpacket_length": subpacket_length}, bits[16:])
    else:
        subpacket_count = int(bits[1:12], 2)
        return ({"length_type": length_type_id, "subpacket_count": subpacket_count}, bits[12:])



def parse_literal(bits):
    current = ""
    new_bits = bits
    while True:
        chunk = new_bits[:5]
        new_bits = new_bits[5:]
        current = current + chunk[1:]

        if chunk[0] == '0':
            return (int(current,2), new_bits)

def to_bits(v):
    return ''.join(["{0:04b}".format(int(c, 16)) for c in v])


operator_types = {
    0: "sum",
    1: "prod",
    2: "min",
    3: "max",
    4: "literal",
    5: ">",
    6: "<",
    7: "=="
}

def evaluate(op, l):
    if op == "sum":
        return sum(l)
    if op == "prod":
        return math.prod(l)
    if op == "max":
        return max(l)
    if op == "min":
        return min(l)
    if op == ">":
        return 1 if l[0] > l[1] else 0
    if op == "<":
        return 1 if l[0] < l[1] else 0
    if op == "==":
        return 1 if l[0] == l[1] else 0


def parse_sub_packets(bits, level=0, expression_count=None):
    current_count = 0
    current_expression = ""
    current_expressions = []


    while len(bits) >= 8 and (expression_count is None or current_count < expression_count ):
        current_count = current_count + 1

        header, bits = parse_header(bits)

        op = operator_types[header["type"]]
        if op == "literal":
            val, bits = parse_literal(bits)
            current_expression = current_expression + f" {val} "
            current_expressions.append(val)
        else:
            operator_header, bits = parse_operator_header(bits)
            if "subpacket_count" in operator_header:
                exp, l, bits = parse_sub_packets(bits, level + 1, operator_header["subpacket_count"])
                current_expression = current_expression + f" ({op} {exp}) "
                current_expressions.append(evaluate(op, l))
            else:
                sub_bits = bits[:operator_header["subpacket_length"]]
                exp, l, _ = parse_sub_packets(sub_bits, level + 1)
                current_expression = current_expression + f" ({op} {exp}) "
                bits = bits[operator_header["subpacket_length"]:]
                current_expressions.append(evaluate(op, l))


    return (current_expression, current_expressions, bits)



examples = [
"C200B40A82",
"04005AC33890",
"880086C3E88112",
"CE00C43D881120",
"D8005AC2A8F0",
"F600BC2D8F",
"9C005AC2F8F0",
"9C0141080250320F1802104A08"
]

for ex in examples:
    bits = to_bits(ex)
    print(parse_sub_packets(bits))
    print()



input_bits = to_bits(puzzle_input)
print(parse_sub_packets(input_bits))
