import itertools


def generate_code_snippet(codes: list, subroutine_name: str, subroutine_arguments: str):
    l = [False, True]
    iter = list(itertools.product(l, repeat=len(codes)))
    code_section = ""
    for combo in iter:
        tmp = ' .and. '.join([f"{code}" if combo[i] else f".not. {code}" for i, code in enumerate(codes)])
        conditional_statement = f"else if ({tmp}) then"
        subroutine_kwargs = ','.join([f"{code.replace('is_', '')}={code.replace('is_', '')}" for i, code in enumerate(codes) if combo[i]])
        
        code_section += f"{conditional_statement}\n    call {subroutine_name}({subroutine_arguments},{subroutine_kwargs},kout=kout)\n"

    return code_section

codes = ['is_spg', 'is_iphase', 'is_magext', 'is_kinfo', 'is_order', 'is_unique', 'is_seqindx', 'is_hlim', 'is_mag_only', 'is_friedel', 'is_ref_typ']
code = generate_code_snippet(codes, 'gener_reflections', 'cell,slmin,slmax,reflex')

with open('kwargs_permute.txt','w') as f:
    f.write(code)
