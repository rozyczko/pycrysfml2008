from pycrysfml08 import py_cfml_reflections, py_cfml_gspacegroups, py_cfml_metrics
import itertools
import numpy as np


def test_combos(codes: list, variables: list):
    assert len(codes) == len(variables)
    l = [False, True]
    iter = list(itertools.product(l, repeat=len(codes)))
    abc = np.array([4.0,4.0,6.0],dtype=np.float32)
    albega = np.array([90.0,90.0,120.0],dtype=np.float32)
    err,err_mess,cell = py_cfml_metrics.set_crystal_cell(abc,albega,{"cartype":"CA"})
    for combo in iter:
        index = [i for i, val in enumerate(combo) if val]
        arguments = {codes[idx]: variables[idx] for idx in index}
        err,err_mess,reflex,kout = py_cfml_reflections.gener_reflections(cell, 0.2, 0.6, arguments)
        if err != 0:
            print(err_mess)
            print(arguments)

codes = ['spg', 'iphase', 'magext', 'kinfo', 'order', 'unique', 'seqindx', 'hlim', 'mag_only', 'friedel', 'ref_typ']

# Initialize variables
err,err_mess,spg = py_cfml_gspacegroups.set_spacegroup("SPG Pnma")
iphase = 1
magext = [True, False]
err,err_mess,kinfo = py_cfml_gspacegroups.allocate_kvector(2,3)
order = [True, False]
unique = [True, False]
seqindx = np.array([1, 1, 1], dtype=np.int32)
hlim = np.array([[1, 1],[1, 1],[1, 1]], dtype=np.int32)
mag_only = [True, False]
friedel = [True, False]
ref_typ = ['SRefl','MRefl','Refl']

variables = [spg, iphase, magext, kinfo, order, unique, seqindx, hlim, mag_only, friedel, ref_typ]

test_combos(codes, variables)