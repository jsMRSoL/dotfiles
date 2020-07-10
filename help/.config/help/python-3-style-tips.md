Syntax is python3
# Looping over a range of numbers #

```python
for i in range(6):
    print(i**2)
```
is better than
```python
for i in [0, 1, 2, 4, 5]:
    print(i**2)
```
because range generates one number per loop iteration,
conserving memory.
Python's 'for' is really a 'for each'.

    [[http://www.vim.org][vim]]

```python
for i in range(6):
    print(i)
```

