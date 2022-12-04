with open('input') as file:
    input = file.read()

raw = input.split('\n\n')
raw[:] = [elf.split('\n') for elf in raw]

elfs = []

for elf in raw:
    elf = [int(e) for e in elf]
    elfs.append(sum(elf))

elfs.sort(reverse=True)

print(f'Top elf: {elfs[0]}')
print(f'Top three elfs: {sum(elfs[:3])}')
