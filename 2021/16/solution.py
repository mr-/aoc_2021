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


def parse_sub_packets(bits, limit={}):
    current_count = 0
    current_expression = ""
    current_expressions = []

    if "subpacket_length" in limit:
        return_bits = bits[limit["subpacket_length"]:]
        bits = bits[:limit["subpacket_length"]]

    while len(bits) >= 8 and ((not "subpacket_count" in limit) or current_count < limit["subpacket_count"] ):
        current_count = current_count + 1

        header, bits = parse_header(bits)

        op = operator_types[header["type"]]
        if op == "literal":
            val, bits = parse_literal(bits)
            current_expression = current_expression + f" {val} "
            current_expressions.append(val)
        else:
            operator_header, bits = parse_operator_header(bits)
            exp, l, bits = parse_sub_packets(bits,  operator_header)
            current_expression = current_expression + f" ({op} {exp}) "
            current_expressions.append(evaluate(op, l))


    return (current_expression, current_expressions, return_bits if "subpacket_length" in limit else bits)



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


# output:

# (' (sum  1  2 ) ', [3], '')
# (' (prod  6  9 ) ', [54], '0000')
# (' (min  7  8  9 ) ', [7], '0')
# (' (max  7  8  9 ) ', [9], '00000')
# (' (<  5  15 ) ', [1], '0000')
# (' (>  5  15 ) ', [0], '')
# (' (==  5  15 ) ', [0], '0000')
# (' (==  (sum  1  3 )  (prod  2  2 ) ) ', [1], '00')
# (' (sum  (prod  9  (<  90  258 ) )  (max  14  2 )  (min  15083  4  10 )  (min  46486  946644 )  47731  (prod  86  167  76  41  75 )  (prod  902426  (<  5448597  5448597 ) )  12  3150483273  46921  1756  (prod  3098  (==  241  11104915 ) )  (prod  (>  2338  2338 )  185954532 )  (prod  105  146  64  124 )  1619  (prod  183  (<  440674  103 ) )  (max  6937079  92140  209  592 )  (max  (prod  (min  (min  (max  (sum  (max  (prod  (max  (sum  (min  (sum  (sum  (min  (max  (sum  (max  (prod  (min  (min  5056208039 ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )  455  256083  (sum  (prod  9  6  4 )  (prod  14  4  8 )  (prod  5  3  12 ) )  (min  11  208  70  1647  130 )  (prod  3586  (>  (sum  14  2  7 )  (sum  5  6  3 ) ) )  (prod  1045  (==  7539326  139 ) )  (prod  28505  (<  1344  46 ) )  (prod  (==  310846  310846 )  55133606844 )  2158747088  (max  50614281761 )  (prod  44 )  (prod  (sum  10  15  9 )  (sum  8  10  11 )  (sum  8  7  9 ) )  (prod  40917  (<  206  206 ) )  (prod  152  (>  664377  115979877 ) )  (prod  140  (<  (sum  10  14  15 )  (sum  13  6  7 ) ) )  (sum  3  204  122 )  (max  277  62608  143  3234  3179 )  (min  188615  4835  64264  685172 )  (prod  (>  (sum  9  11  15 )  (sum  9  7  10 ) )  7877750 )  (prod  22  (==  (sum  15  15  4 )  (sum  11  9  12 ) ) )  (prod  (>  31223  31223 )  126 )  (max  139455  2126  1316140047 )  (sum  57417  5 )  (prod  (>  2997  24470 )  177 )  13  (prod  97  128 )  (sum  85  37726  178  7  277352178 )  (prod  3851  (>  233881589634  2158 ) )  (sum  15 )  (prod  8  (<  6  2589 ) )  (prod
