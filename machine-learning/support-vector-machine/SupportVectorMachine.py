import numpy as np

import subprocess
import pathlib

def support_vector_machine(objects: np.array, results: np.array, c: int):
    executablePath = "%s/support-vector-machine" % pathlib.Path(__file__).parent.absolute()
    p = subprocess.Popen(executablePath, stdin=subprocess.PIPE, stdout=subprocess.PIPE, shell=True)

    input = []
    input.append(str(objects.shape[0]))

    for i in range(len(objects)):
        input.append("\n")
        for j in objects[i]:
            input.append(str(j))
            input.append(" ")
        input.append(str(results[i]))
    input.append("\n")
    input.append(str(c))

    # return ''.join(input)
    input = ''.join(input).encode('utf-8')

    p.stdin.write(input)
    (output, _) = p.communicate()

    result = list(map(float, ''.join(map(chr, output)).split('\n')[:-1]))

    return (result[:-1], result[-1])

