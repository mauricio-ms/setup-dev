#!/bin/bash

# Generate random CPF using Python
CPF=$(python3 -c "
import random
def calc_dig(digs):
    s = sum((len(digs)+1-i)*int(v) for i,v in enumerate(digs))
    r = s % 11
    return '0' if r < 2 else str(11 - r)

nove = [str(random.randint(0,9)) for _ in range(9)]
d1 = calc_dig(nove)
d2 = calc_dig(nove + [d1])
cpf = ''.join(nove + [d1, d2])
print(f'{cpf[:3]}.{cpf[3:6]}.{cpf[6:9]}-{cpf[9:]}')
")

# Copy to clipboard
echo -n "$CPF" | xclip -selection clipboard
