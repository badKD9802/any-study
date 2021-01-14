#파이썬 함수

def add(a,b):
    return a + b

#여러 개의 입력을 처리할 때

def add_mul(choice,*args):
    if choice == "add":
        result = 0
        for i in args:
            result = result + i
    elif choice == "mul":
        result = 1
        for i in args:
            result = result*i
    return result

result = add_mul('add',1,2,3,4,5)
print(result)
result = add_mul('mul',1,2,3,4,5)
print(result)

##여러개의 입력값

def add_and_mul(a,b):
    return a+b, a*b

result = add_and_mul(3,4)
print(result)
