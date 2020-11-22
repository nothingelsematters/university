import subprocess
import pathlib

def naive_bayes_classifier(class_amount: int, fines: list, alpha: float, learning: list, testing: list):
    executablePath = "%s/naive-bayes-classifier" % pathlib.Path(__file__).parent.absolute()
    p = subprocess.Popen(executablePath, stdin=subprocess.PIPE, stdout=subprocess.PIPE, shell=True)

    input_string = [
        str(class_amount),
        "\n",
        " ".join([str(x) for x in fines]),
        "\n",
        str(alpha),
        "\n",
        str(len(learning))
    ]

    for class_number, words in learning:
        input_string += "\n" + " ".join([str(class_number), str(len(words))] + [str(x) for x in words])

    input_string += ["\n", str(len(testing))]

    for words in testing:
        input_string += "\n" + " ".join([str(len(words))] + [str(x) for x in words])

    input_string = ''.join(input_string).encode('utf-8')

    p.stdin.write(input_string)
    (output, _) = p.communicate()

    return [list(map(float, filter(lambda  x: len(x) > 0, x.split(' ')))) for x in ''.join(map(chr, output)).split('\n')[:-1]]
