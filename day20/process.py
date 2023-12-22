# %%
print("digraph day20 {")
for line in open ("input.txt","r"):
    a,b = line.strip().split(" -> ")
    ty = a[0]
    lab = a[1:]
    out = b.split(", ")
    print(f'{lab}[label="{a}"]')
    for nm in out:
        print(f'{lab} -> {nm};')
print("}")

# %%
