## 基本操作



**for while if elif  list dict  touple   def** 

[编程语言排行榜](https://www.tiobe.com/tiobe-index/)

[官方文档](https://docs.python.org/zh-cn/3.9/)



### 基本数据结构

#### 字符串

字符串有多种表现形式，用单引号（`'……'`）或双引号（`"……"`）标注的结果相同，反斜杠 `\` 用于转义

```python
'"Yes," they said.'
'doesn\'t' 
print('"Isn\'t," they said.')
print('C:\some\name') 
print(r'C:\some\name')

3 * 'un' + 'ium'

word = 'Python'
word[0]
word[-1] 

# 切片索引的默认值很有用；省略开始索引时，默认值为 0，省略结束索引时，默认为到字符串的结尾：

word[0:2]
```



- 三重引号: `'''三重单引号'''`, `"""三重双引号"""`

  ```python
  >>> work = '''23r3333242
  ... 324324
  ... 2434234242
  ... 4324324
  ... 4324324'''
  >>> work
  '23r3333242\n324324\n2434234242\n4324324\n4324324'
  >>>
  
  ```

字符串格式化输出： 

```python
>>> print('test str  %s ,test num %d'%('python',555))
test str  python ,test num 555
```

格式化输出： 

```python
>>> text = '{0}, {1}, {2}'.format('a', 'b', 'c')
>>> print(text)
a, b, c
```



#### if

```python
def test_if():
    x = int(input("Please enter an integer: "))
    if x < 0:
        x = 0
        print('Negative changed to zero')
    elif x == 0:
        print('Zero')
    elif x == 1:
        print('Single')
    else:
        print('More')
```



#### `for`、 `break`、`continue`

```python
def test_for():
    words = ['cat', 'window', 'defenestrate']
    for w in words:
        print(w, len(w))


def test_for_2():
    users = {"John": "inactive",
             "Helen": "active",
             "James": "active",  # and so on...
             }
    for user, status in users.copy().items():
        if status == 'inactive':
            del users[user]
            print(users)


def test_break():
    for num in range(1, 10):
        print('num is %d' % num)
        if num == 5:
            break


def test_continue():
    for num in range(2, 10):
        if num % 2 == 0:
            print("Found an even number", num)
            continue
        print("Found an odd number", num)


```



[字符串函数操作](https://docs.python.org/zh-cn/3.9/library/stdtypes.html#str.capitalize)

#### 列表list

类似Java中的list

Python 支持多种 *复合* 数据类型，可将不同值组合在一起。最常用的 *列表* ，是用方括号标注，逗号分隔的一组值。*列表* 可以包含不同类型的元素，但一般情况下，各个元素的类型相同：

```python
def test_list():
    letters = ['a', 'b', 'c', 'd', 'e', 'f', 'g']
    print(letters)
    letters[2:5] = ['C', 'D', 'E']
    print(letters)
    letters[2:5] = []
    print(letters)
    letters[:] = []
    print(letters)
    print(len(letters))


# 嵌套列表
def list_in():
    a = ['a', 'b', 'c']
    n = [1, 2, 3]
    x = [a, n]
    print(x)
    print(x[0])
    print(x[0][1])
```



```python
""" 列表相关函数 """

from collections import deque
from math import pi


def test_list():
    """ insert、remove、sort 等方法只修改列表，不输出返回值——返回的默认值为 None 。
    1 这是所有 Python 可变数据结构的设计原则。 """
    fruits = ['orange', 'apple', 'pear', 'banana', 'kiwi', 'apple', 'banana']
    print(fruits.count('apple'))
    print(fruits.count('tangerine'))
    print(fruits.index('banana'))
    # Find next banana starting a position 4
    print(fruits.index('banana', 4))
    fruits.reverse()
    print(fruits)
    fruits.append('grape')
    print(fruits)
    fruits.sort()
    print(fruits)
    # 从列表最后移除一个元素
    fruits.pop()
    print(fruits)


def test_list_stack():
    """ 用列表实现栈的操作 """
    stack = [3, 4, 5]
    print(stack)
    stack.append(6)
    stack.append(7)
    print(stack)
    print(stack.pop())
    print(stack)


def test_deque():
    queue = deque(["Eric", "John", "Michael"])
    queue.append("Terry")
    queue.append("Graham")
    print(queue.popleft())
    print(queue.popleft())
    print(queue)


def test_list_2():
    squares = []
    for x in range(10):
        squares.append(x ** 2)
    print(squares)


def test_list_3():
    squares = [x ** 2 for x in range(10)]
    print(squares)


def test_list_4():
    """列表推导式的方括号内包含以下内容：一个表达式，后面为一个 for 子句，
    然后，是零个或多个 for 或 if 子句。结果是由表达式依据 for 和 if 子句求值计算而得出一个新列表。
    举例来说，以下列表推导式将两个列表中不相等的元素组合起来："""
    print([(x, y) for x in [1, 2, 3] for y in [3, 1, 4] if x != y])


def test_list_5():
    """ 4是推导式的python化的写法，  test_list_5是test_list_4的简单翻译"""
    temp = []
    for i in [1, 2, 3]:
        for j in [3, 1, 4]:
            if i != j:
                temp.append((i, j))
    print(temp)


def test_list_6():
    print([str(round(pi, i)) for i in range(1, 6)])


def test_list_del():
    """ del 语句按索引，而不是值从列表中移除元素。与返回值的 pop() 方法不同， del 语句也可以从列表中移除切片，或清空整个列表（之前是将空列表赋值给切片）。 例如：
        """
    a = [-1, 1, 66.25, 333, 333, 1234.5]
    print(a)
    del a[0]
    print(a)
    del a[2:4]
    print(a)
    del a[:]
    print(a)


def test_list_7():
    """  行转列 """
    matrix = [
        [1, 2, 3, 4],
        [5, 6, 7, 8],
        [9, 10, 11, 12],
    ]
    print([[row[i] for row in matrix] for i in range(4)])


if __name__ == "__main__":
    test_list_del()
```





[列表常见操作说明](https://docs.python.org/zh-cn/3.9/tutorial/datastructures.html)



#### 元组touple

```python
""" 元组相关 , 元组由多个用逗号隔开的值组成   输出时，元组都要由圆括号标注，这样才能正确地解释嵌套元组。   输入时，圆括号可有可无，不过经常是必须的（如果元组是更大的表达式的一部分）。   不允许为元组中的单个元素赋值，当然，可以创建含列表等可变对象的元组。    虽然，元组与列表很像，但使用场景不同，用途也不同。    元组是 immutable （不可变的），一般可包含异质元素序列，通过解包（见本节下文）或索引访问    （如果是 namedtuples，可以属性访问）。列表是 mutable （可变的），    列表元素一般为同质类型，可迭代访问。"""def test_tuple():    t = 12345, 54321, 'hello!'    print(t[0])    print(t)    u = t, (1, 2, 3, 4, 5)    print(u)    print(u[0])    # 元组定义好以后，不能改变    # t[0] = 88888if __name__ == "__main__":    test_tuple()
```





#### 字典dict

```python
""" 字典 （参见 映射类型 --- dict） 也是一种常用的 Python 內置数据类型。其他语言可能把字典称为 联合内存    或 联合数组。与以连续整数为索引的序列不同，字典以 关键字 为索引，关键字通常是字符串或数字，    也可以是其他任意不可变类型。只包含字符串、数字、元组的元组，也可以用作关键字。    但如果元组直接或间接地包含了可变对象，就不能用作关键字。列表不能当关键字，因为列表可以用索引、切片、append()    、extend() 等方法修改。    可以把字典理解为 键值对 的集合，但字典的键必须是唯一的。    花括号 {} 用于创建空字典。另一种初始化字典的方式是，在花括号里输入逗号分隔的键值对，这也是字典的输出方式。    字典的主要用途是通过关键字存储、提取值。用 del 可以删除键值对。    用已存在的关键字存储值，与该关键字关联的旧值会被取代。通过不存在的键提取值，则会报错。    对字典执行 list(d) 操作，返回该字典中所有键的列表，按插入次序排列（如需排序，请使用 sorted(d)）。    检查字典里是否存在某个键，使用关键字 in。"""def test_dic_01():    tel = {'jack': 4098, 'sape': 4139}    print(tel)    tel['guido'] = 4127    print(tel)    del tel['sape']    print(tel)    tel['irv'] = 4127    print(tel)    print(list(tel))    sorted(tel)    print(tel)    print('guido' in tel)    print('jack' not in tel)    temp = dict(sape=4139, guido=4127, jack=4098)    print(temp)def test_loop_02():    knights = {'gallahad': 'the pure', 'robin': 'the brave'}    for k, v in knights.items():        print(k, v)    # 在序列中循环时，用 enumerate() 函数可以同时取出位置索引和对应的值：    for i, v in enumerate(['tic', 'tac', 'toe']):        print(i, v)    questions = ['name', 'quest', 'favorite color']    answers = ['lancelot', 'the holy grail', 'blue']    for q, a in zip(questions, answers):        print('What is your {0}?  It is {1}.'.format(q, a))    for i in reversed(range(1, 10, 2)):        print(i)    # 使用 set() 去除序列中的重复元素。使用 sorted() 加 set() 则按排序后的顺序，循环遍历序列中的唯一元素：    basket = ['apple', 'orange', 'apple', 'pear', 'orange', 'banana']    for f in sorted(set(basket)):        print(f)if __name__ == '__main__':    test_loop_02()
```



#### 注释

##### 单行注释 

Python 注释以 `#` 开头，直到该物理行结束。注释可以在行开头，或空白符与代码之后，但不能在字符串里面。字符串中的 # 号就是 # 号。注释用于阐明代码，Python 不解释注释，键入例子时，可以不输入注释

```python
# 这是一行注释check = Trueclass MyClass:    """this is class note"""    @staticmethod    def printSay():        """print say welcome to you."""        print('say welcome to you.')def printNote():    print(MyClass.__doc__)    print(MyClass.printSay.__doc__)    print(mym.__doc__)    print(mym.f.__doc__)
```



#### 交互式模式： 

```python
>>> world = True>>> if world:...     print("3322")...3322
```

将中断字符（通常为 Control-C 或 Delete ）键入主要或辅助提示会取消输入并返回主提示符。 [1](https://docs.python.org/zh-cn/3.9/tutorial/appendix.html#id2) 在执行命令时键入中断引发的 [`KeyboardInterrupt`](https://docs.python.org/zh-cn/3.9/library/exceptions.html#KeyboardInterrupt) 异常，可以由 [`try`](https://docs.python.org/zh-cn/3.9/reference/compound_stmts.html#try) 语句处理。

退出交互式模式执行 `exit()`函数



### 函数

函数定义，形参，返回值，获取函数内定义，比如 __doc__

```python
def test_func():    """ this is doc 注意方法的注释 和java 不同，写在方法内部，可以通过 __doc__ 获取"""if __name__ == '__main__':    print(test_func.__doc__)    # fib(1000)        if __name__ == '__main__':    """函数的引用"""    test_func2 = test_func    test_func2()    """ function  test """import MyModule as mymdef test_func():    """ this is doc 注意方法的注释 和java 不同，写在方法内部，可以通过 __doc__ 获取"""class MyClass:    """this is class note"""    @staticmethod    def printSay():        """print say welcome to you."""        print('say welcome to you.')def printNote():    print(MyClass.__doc__)    print(MyClass.printSay.__doc__)    print(mym.__doc__)    print(mym.f.__doc__)def fib(n):    """Print a Fibonacci series up to n."""    a, b = 0, 1    while a < n:        print(a, end=' ')        a, b = b, a + b    print()def fib2(n):    """Return a list containing the Fibonacci series up to n."""    result = []    a, b = 0, 1    while a < n:        result.append(a)  # see below        a, b = b, a + b    return resultdef printFib2():    # 无返回值结果返回None    resultNone = fib(100)    print(resultNone)    result = fib2(100)    print(result)def ask_ok(prompt, retries=4, reminder='Please try again!'):    """定义可变参数"""    while True:        ok = input(prompt)        if ok in ('y', 'ye', 'yes'):            return True        if ok in ('n', 'no', 'nop', 'nope'):            return False        retries = retries - 1        if retries < 0:            raise ValueError('invalid user response')        print(reminder)def invoke_ask_ok():    # ask_ok('y')    # ask_ok('y', 3)    ask_ok('fdd', 5, '请重新输入')def f(a, L=[]):    L.append(a)    return Ldef printF():    print(f(1))    print(f(2))    print(f(3))def fNone(a, L=None):    if L is None:        L = []    L.append(a)    return Ldef printNone():    print(fNone(1))    print(fNone(2))    print(fNone(3))def parrot(voltage, state='a stiff', action='voom', type='Norwegian Blue'):    """关键字参数"""    print("-- This parrot wouldn't", action, end=' ')    print("if you put", voltage, "volts through it.")    print("-- Lovely plumage, the", type)    print("-- It's", state, "!")def invoke_parrot():    parrot(1000)  # 1 positional argument    print('------------------------------------')    parrot(voltage=1000)  # 1 keyword argument    print('------------------------------------')    parrot(voltage=1000000, action='VOOOOOM')  # 2 keyword arguments    print('------------------------------------')    parrot(action='VOOOOOM', voltage=1000000)  # 2 keyword arguments    print('------------------------------------')    parrot('a million', 'bereft of life', 'jump')  # 3 positional arguments    print('------------------------------------')    parrot('a thousand', state='pushing up the daisies')  # 1 positional, 1 keyword       if __name__ == '__main__':    invoke_parrot()    
```





````python
def out_func(a, b):    def func(x):        return a*x+b    return funcif __name__ == '__main__':    func2 = out_func(3, 5)    func3 = out_func(5, 10)    print(func2(2))    print(func3(5))
````







#### 可变参数VS关键字参数

**可变参数允许你传入0个或任意个参数，这些可变参数在函数调用时自动组装为一个tuple,而关键字参数允许你传入0个或任意个含参数名的参数，这些关键字参数在函数内部自动组装为一个dict**

可变参数 `*args ` 和 关键字参数  `**kw`



`*args`是可变参数，`args`接收的是一个`tuple`；

`**kw`是关键字参数，`kw`接收的是一个`dict`。

默认参数一定要用不可变对象，如果是可变对象，程序运行时会有逻辑错误！

```python
def total_sum(num):    total = 0    for i in num:        total += i    return totaldef test_total_sum():    numbers = [1, 2, 3, 4, 5]    print(total_sum(numbers))def person(name, age, **kw):    """关键字参数"""    print('name:', name, 'age:', age, 'other:', kw)def test_person():    person('hero', 18)    person('frank', 20, gender='M', job='Engineer', city='beijing')    """**extra表示把extra这个dict的所有key-value用关键字参数传入到函数的**kw参数，kw将获得一个dict，    注意kw获得的dict是extra的一份拷贝，对kw的改动不会影响到函数外的extra"""    extra = {'city': 'Beijing', 'job': 'Engineer'}    person('tom', 18, **extra)def multiple(a, b, c=0, *args, **kw):    """ 在Python中定义函数，可以用必选参数、默认参数、可变参数、关键字参数和命名关键字参数，这5种参数都可以组合使用，    除了可变参数无法和命名关键字参数混合。但是请注意，参数定义的顺序必须是：必选参数、默认参数、可变参数/命名关键字参数    和关键字参数。"""    print('a =', a, 'b =', b, 'c =', c, 'args =', args, 'kw =', kw)def test_multiple():    multiple(1, 2)    multiple(1, 2, 3)    multiple(1, 2, 3, 'a', 'b')    multiple(1, 2, 3, 'a', 'b', x=99)    args = (1, 2, 3, 4)    kw = {'d': 99, 'x': 'u'}    multiple(args, kw)def concat(*args, sep="/"):    return sep.join(args)def test_concat():    print(concat('tom', 'cat', 'fine'))    print(concat('tom', 'cat', 'fine', sep="."))def parrot(voltage, state='a stiff', action='voom'):    """ 字典中相关参数，可以直接传入，覆盖自定义参数 """    print("-- This parrot wouldn't", action, end=' ')    print("if you put", voltage, "volts through it.", end=' ')    print("E's", state, "!")def test_parrot():    d = {"voltage": "four million", "state": "bleedin' demised", "action": "VOOM"}    parrot(**d)    #d2 = {"state": "bleedin' demised", "action": "VOOM"}    #parrot(**d2)    d3 = {"voltage": "four million",  "action": "VOOM"}    parrot(**d3)
```





最后一个形参为 `**name` 形式时，接收一个字典（详见 [映射类型 --- dict](https://docs.python.org/zh-cn/3.9/library/stdtypes.html#typesmapping)），该字典包含与函数中已定义形参对应之外的所有关键字参数。`**name` 形参可以与 `*name` 形参（下一小节介绍）组合使用（`*name` 必须在 `**name` 前面）， `*name` 形参接收一个 [元组](https://docs.python.org/zh-cn/3.9/tutorial/datastructures.html#tut-tuples)，该元组包含形参列表之外的位置参数。例如，可以定义下面这样的函数：



#### Lambda 表达式

```python
def make_incrementor(n):    return lambda x: x + ndef test_lambda():    lam = make_incrementor(80)    print(lam(5))    print(lam(10))
```



###  闭包  



```python
def counter(FIRST=0):    cnt = [FIRST]    def add_one():        cnt[0] += 1        return cnt[0]    return add_onenum5 = counter(5)num10 = counter(10)print(num5())print(num5())print(num5())print(num10())print(num10())
```





### 装饰器用法

```python
import timedef cost(func):    def cost_handle():        start_time = time.time()        func()        stop_time = time.time()        print("func cost  %s s " % (stop_time - start_time))    return cost_handle@costdef func_handle():    time.sleep(3)@costdef func_handle_2():    print('start handle')    time.sleep(5)    print('end handle')if __name__ == '__main__':    func_handle()    func_handle_2()
```









### 异常



[`try`](https://docs.python.org/zh-cn/3.9/reference/compound_stmts.html#try) 语句的工作原理如下：

- 首先，执行 *try 子句* （[`try`](https://docs.python.org/zh-cn/3.9/reference/compound_stmts.html#try) 和 [`except`](https://docs.python.org/zh-cn/3.9/reference/compound_stmts.html#except) 关键字之间的（多行）语句）。

- 如果没有触发异常，则跳过 *except 子句*，[`try`](https://docs.python.org/zh-cn/3.9/reference/compound_stmts.html#try) 语句执行完毕。

- 如果执行 try 子句时发生了异常，则跳过该子句中剩下的部分。如果异常的类型与 [`except`](https://docs.python.org/zh-cn/3.9/reference/compound_stmts.html#except) 关键字后面的异常匹配，则执行 except 子句，然后，继续执行 [`try`](https://docs.python.org/zh-cn/3.9/reference/compound_stmts.html#try) 语句之后的代码。

- 如果发生的异常不是 except 子句中列示的异常，则将其传递到外部的 [`try`](https://docs.python.org/zh-cn/3.9/reference/compound_stmts.html#try) 语句中；如果没有找到处理程序，则它是一个 *未处理异常*，语句执行将终止，并显示如上所示的消息。

  [`try`](https://docs.python.org/zh-cn/3.9/reference/compound_stmts.html#try) 语句可以有多个 except 子句，可为不同异常指定相应的处理程序。但最多只会执行一个处理程序。处理程序只处理对应的 try 子句中发生的异常，而不处理同一 `try` 语句内其他处理程序中的异常。except 子句可以用元组命名多个异常，例如：

```python
def input_exception():    try:        year = int(input('input year info:'))        print('input value is ', year)    except ValueError:        print('year must be num')def divide_exception():    try:        print(1 / 'a')    except Exception as e:        print(' %s' % e)def multiple_exception():    try:        year = int(input('year info'))        print(year)    except (ValueError, AttributeError, KeyError) as e:        print("occur exception " %e)def raise_test():    try:        raise NameError('name info')    except NameError:        print('customer error nameError')def test_close():    try:        f = open("file", "w")        try:            f.write('Hello World!')        finally:            f.close()    except IOError:        print('oops!')def test_with():    try:        with open("output", "w") as outfile:            outfile.write('Hello World')    except IOError:        print('oops!')if __name__ == '__main__':    test_with()
```



[系统内置异常](https://docs.python.org/zh-cn/3/library/exceptions.html)



#### main方法执行



```python
import matplotlib.pyplot as plt# 生成直方图def main():    plt.figure(1, dpi=50)    data = [1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1]    plt.hist(data)    plt.show()if __name__ == "__main__":    main()# python3 mataplotlib.py
```

如果没有执行权限，可以使用**chmod** 命令为脚本提供可执行模式或权限。

`chmod +x myscript.py`

`__name__`属性 用于判断当前模块是不是程序入口，如果当前程序正在使用，__name__的值为__main__



### 编码规范

[PEP8编码规范](https://www.python.org/dev/peps/pep-0008/)

Python 项目大多都遵循 [**PEP 8**](https://www.python.org/dev/peps/pep-0008) 的风格指南；它推行的编码风格易于阅读、赏心悦目。Python 开发者均应抽时间悉心研读；以下是该提案中的核心要点：

- 缩进，用 4 个空格，不要用制表符。

  4 个空格是小缩进（更深嵌套）和大缩进（更易阅读）之间的折中方案。制表符会引起混乱，最好别用。

- 换行，一行不超过 79 个字符。

  这样换行的小屏阅读体验更好，还便于在大屏显示器上并排阅读多个代码文件。

- 用空行分隔函数和类，及函数内较大的代码块。

- 最好把注释放到单独一行。

- 使用文档字符串。

- 运算符前后、逗号后要用空格，但不要直接在括号内使用： `a = f(1, 2) + g(3, 4)`。

- 类和函数的命名要一致；按惯例，命名类用 `UpperCamelCase`，命名函数与方法用 `lowercase_with_underscores`。命名方法中第一个参数总是用 `self` (类和方法详见 [初探类](https://docs.python.org/zh-cn/3.9/tutorial/classes.html#tut-firstclasses))。

- 编写用于国际多语环境的代码时，不要用生僻的编码。Python 默认的 UTF-8 或纯 ASCII 可以胜任各种情况。

- 同理，就算多语阅读、维护代码的可能再小，也不要在标识符中使用非 ASCII 字符。





### 模块

Fib  模块

```python
def fib(n):    """ write Fibonacci series up to n"""    a, b = 0, 1    while a < n:        print(a, end=' ')        a, b = b, a+b    print()def fib2(n):    """  return Fibonacci series up to n"""    result = []    a, b = 0, 1    while a < n:        result.append(a)        a, b = b, a+b    return result
```







```python
# import  Fib#from Fib import fib, fib2"""这种方式会导入所有不以下划线（_）开头的名称。大多数情况下，不要用这个功能，这种方式向解释器导入了一批未知的名称，可能会覆盖已经定义的名称。注意，一般情况下，不建议从模块或包内导入 *， 因为，这项操作经常让代码变得难以理解。不过，为了在交互式编译器中少打几个字，这么用也没问题。模块名后使用 as 时，直接把 as 后的名称与导入模块绑定。"""# from Fib import *import Fib as fibif __name__ == '__main__':    fib.fib(10)    # dir查询包中有哪些方法    print(dir(fib))
```



#### dir函数的使用

内置函数 [`dir()`](https://docs.python.org/zh-cn/3.9/library/functions.html#dir) 用于查找模块定义的名称。返回结果是经过排序的字符串列表：

没有参数时，[`dir()`](https://docs.python.org/zh-cn/3.9/library/functions.html#dir) 列出当前定义的名称：





### 包管理工具pip

![image-20220117171127135](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220117171127135.png)



包是一种用“点式模块名”构造 Python 模块命名空间的方法。例如，模块名 `A.B` 表示包 `A` 中名为 `B` 的子模块。正如模块可以区分不同模块之间的全局变量名称一样，点式模块名可以区分 NumPy 或 Pillow 等不同多模块包之间的模块名称。

假设要为统一处理声音文件与声音数据设计一个模块集（“包”）。声音文件的格式很多（通常以扩展名来识别，例如：`.wav`， `.aiff`， `.au`），因此，为了不同文件格式之间的转换，需要创建和维护一个不断增长的模块集合。为了实现对声音数据的不同处理（例如，混声、添加回声、均衡器功能、创造人工立体声效果），还要编写无穷无尽的模块流。下面这个分级文件树展示了这个包的架构：



包 -->模块--->类 --->函数

Python 只把含 `__init__.py` 文件的目录当成包。这样可以防止以 `string` 等通用名称命名的目录，无意中屏蔽出现在后方模块搜索路径中的有效模块。 最简情况下，`__init__.py` 只是一个空文件，但该文件也可以执行包的初始化代码，或设置 `__all__` 变量，详见下文。

```shell
sound/                          Top-level package      __init__.py               Initialize the sound package      formats/                  Subpackage for file format conversions              __init__.py              wavread.py              wavwrite.py              aiffread.py              aiffwrite.py              auread.py              auwrite.py              ...      effects/                  Subpackage for sound effects              __init__.py              echo.py              surround.py              reverse.py              ...      filters/                  Subpackage for filters              __init__.py              equalizer.py              vocoder.py              karaoke.py              ...
```

还可以从包中导入单个模块，例如：

```
import sound.effects.echo
```

这段代码加载子模块 `sound.effects.echo` ，但引用时必须使用子模块的全名：

```
sound.effects.echo.echofilter(input, output, delay=0.7, atten=4)
```

另一种导入子模块的方法是 ：

```
from sound.effects import echo
```

这段代码还可以加载子模块 `echo` ，不加包前缀也可以使用。因此，可以按如下方式使用：

```
echo.echofilter(input, output, delay=0.7, atten=4)
```

Import 语句的另一种变体是直接导入所需的函数或变量：

```
from sound.effects.echo import echofilter
```

同样，这样也会加载子模块 `echo`，但可以直接使用函数 `echofilter()`：

```
echofilter(input, output, delay=0.7, atten=4)
```

**注意，使用 `from package import item` 时，item 可以是包的子模块（或子包），也可以是包中定义的函数、类或变量等其他名称。`import` 语句首先测试包中是否定义了 item；如果未在包中定义，则假定 item 是模块，并尝试加载。如果找不到 item，则触发 [`ImportError`](https://docs.python.org/zh-cn/3.9/library/exceptions.html#ImportError) 异常。**

**相反，使用 `import item.subitem.subsubitem` 句法时，除最后一项外，每个 item 都必须是包；最后一项可以是模块或包，但不能是上一项中定义的类、函数或变量。**

[包结构参考](https://docs.python.org/zh-cn/3.9/tutorial/modules.html)



![image-20220118104850261](https://typorawqp.oss-cn-beijing.aliyuncs.com/uPic/image-20220118104850261.png)





### 输出

使用 [格式化字符串字面值](https://docs.python.org/zh-cn/3.9/tutorial/inputoutput.html#tut-f-strings) ，要在字符串开头的引号/三引号前添加 `f` 或 `F` 。在这种字符串中，可以在 `{` 和 `}` 字符之间输入引用的变量，或字面值的 Python 表达式。

```python
def print_01():    year = 2016    event = 'Referendum'    print(f'Results of the {year} {event}')if __name__ == '__main__':    print_01()
```

字符串的 [`str.format()`](https://docs.python.org/zh-cn/3.9/library/stdtypes.html#str.format) 方法需要更多手动操作。该方法也用 `{` 和 `}` 标记替换变量的位置，虽然这种方法支持详细的格式化指令，但需要提供格式化信息。

```python
def print_02():    yes_votes = 42_572_654    no_votes = 43_132_495    percentage = yes_votes / (yes_votes + no_votes)    print('{:-9} YES votes  {:2.2%}'.format(yes_votes, percentage))
```

如果不需要花哨的输出，只想快速显示变量进行调试，可以用 [`repr()`](https://docs.python.org/zh-cn/3.9/library/functions.html#repr) 或 [`str()`](https://docs.python.org/zh-cn/3.9/library/stdtypes.html#str) 函数把值转化为字符串。

[`str()`](https://docs.python.org/zh-cn/3.9/library/stdtypes.html#str) 函数返回供人阅读的值，[`repr()`](https://docs.python.org/zh-cn/3.9/library/functions.html#repr) 则生成适于解释器读取的值（如果没有等效的语法，则强制执行 [`SyntaxError`](https://docs.python.org/zh-cn/3.9/library/exceptions.html#SyntaxError)）。对于没有支持供人阅读展示结果的对象， [`str()`](https://docs.python.org/zh-cn/3.9/library/stdtypes.html#str) 返回与 [`repr()`](https://docs.python.org/zh-cn/3.9/library/functions.html#repr) 相同的值。一般情况下，数字、列表或字典等结构的值，使用这两个函数输出的表现形式是一样的。字符串有两种不同的表现形式。

```python
""" 格式化输出 """import mathimport jsondef print_01():    year = 2016    event = 'Referendum'    print(f'Results of the {year} {event}')def print_02():    yes_votes = 42_572_654    no_votes = 43_132_495    percentage = yes_votes / (yes_votes + no_votes)    print('{:-9} YES votes  {:2.2%}'.format(yes_votes, percentage))def print_03():    s = 'Hello, world.'    print(str(s))    print('***'*6)    print(repr(s))    print(str(1/7))    print('***' * 6)    x = 10 * 3.25    y = 200 * 200    s = 'The value of x is ' + repr(x) + ', and y is ' + repr(y) + '...'    print(s)    print('***' * 6)    hello = 'hello, world\n'    hellos = repr(hello)    print(hellos)def print_04():    print(f'The value of pi is approximately {math.pi:.3f}.')    print('***' * 6)    table = {'Sjoerd': 4127, 'Jack': 4098, 'Dcab': 7678}    for name, phone in table.items():        print(f'{name:10} ==> {phone:10d}')def print_05():    print('We are the {} who say "{}!"'.format('knights', 'Ni'))    print('***' * 6)    print('{1} and {0}'.format('spam', 'eggs'))    print('***' * 6)    print('This {food} is {adjective}.'.format(food='spam', adjective='absolutely horrible'))    print('***' * 6)    print('The story of {0}, {1}, and {other}.'.format('Bill', 'Manfred', other='Georg'))    print('***' * 6)    table = {'Sjoerd': 4127, 'Jack': 4098, 'Dcab': 8637678}    print('Jack: {0[Jack]:d}; Sjoerd: {0[Sjoerd]:d}; ''Dcab: {0[Dcab]:d}'.format(table))def print_06():    """ open() 返回 file object，最常用的参数有两个: open(filename, mode)。    第一个实参是文件名字符串。第二个实参是包含描述文件使用方式字符的字符串。mode 的值包括 'r' ，    表示文件只能读取；'w' 表示只能写入（现有同名文件会被覆盖）；'a' 表示打开文件并追加内容，    任何写入的数据会自动添加到文件末尾。'r+' 表示打开文件进行读写。mode 实参是可选的，省略时的默认值为 'r'。    """    with open('workfile', 'r') as f:        print(f.read())def print_07():    """ 在处理文件对象时，最好使用 with 关键字。优点是，子句体结束后，文件会正确关闭，即便触发异常也可以。    而且，使用 with 相比等效的 try-finally 代码块要简短得多："""    with open('workfile') as f:        read_data = f.read()        print(f'file closed status {f.closed}')    print(f'end with file close status {f.closed}')def json_test_01():    person = '{"name": "Bob", "languages": ["English", "French"]}'    person_dict = json.loads(person)    print(person_dict)    print(person_dict['languages'])    person_dict = {'name': 'Bob',                   'age': 12,                   'children': None                   }    person_json = json.dumps(person_dict)    print('***' * 8)    print(person_json)if __name__ == '__main__':    json_test_01()
```



[格式化输出说明](https://docs.python.org/zh-cn/3.9/tutorial/inputoutput.html)



### 作用域scope

```python
def scope_test():    """ 请注意 局部 赋值（这是默认状态）不会改变 scope_test 对 spam 的绑定。    nonlocal 赋值会改变 scope_test 对 spam 的绑定，而 global 赋值会改变模块层级的绑定。    """    def do_local():        spam = "local spam"    def do_nonlocal():        nonlocal spam        spam = "nonlocal spam"    def do_global():        global spam        spam = "global spam"    spam = "test spam"    do_local()    print("After local assignment:", spam)    do_nonlocal()    print("After nonlocal assignment:", spam)    do_global()    print("After global assignment:", spam)def scope_test_t():    scope_test()    print("In global scope:", spam)if __name__ == '__main__':    scope_test_t()
```



### 类

```python
def print_type():    print(type([]))    print(type({}))    print(type(Exception()))    print(type(1))    print(type(''))def print_object_id():    print(id([]))    print(id({}))    print(id(Exception()))    print(id(1))    print(id(''))class MyClass:    """A simple example class  实例化操作（“调用”类对象）会创建一个空对象。     许多类喜欢创建带有特定初始状态的自定义实例。     为此类定义可能包含一个名为 __init__() 的特殊方法，就像这样:    """    i = 12345    def __init__(self):        """当一个类定义了 __init__() 方法时，类的实例化操作会自动为新创建的类实例发起调用 __init__()。         因此在这个示例中，可以通过以下语句获得一个经初始化的新实例:        """        self.data = []    def f(self):        return 'hello world'def test_obj():    obj = MyClass()    print(obj.f())    print(MyClass.i)    print(obj.__doc__)class Complex:    def __init__(self, left, right):        self.r = left        self.i = rightdef test_complex():    x = Complex(3.0, -4.5)    print(f'left value is {x.r}, right value is {x.i} ')class Dog:    """ 类变量赋值，相当于java中的静态变量 """    kind = 'canine'  # class variable shared by all instances    def __init__(self, name):        self.name = name  # instance variable unique to each instancedef test_dog():    """ 类变量赋值，相当于java中的静态变量 """    d = Dog('Fido')    e = Dog('Buddy')    # 类的变量    print(d.kind)    print(e.kind)    # 实例变量    print(d.name)    print(e.name)class Dog2:    # tricks = []             # mistaken use of a class variable    """ 方法的第一个参数常常被命名为 self。 这也不过就是一个约定: self 这一名称在 Python 中绝对没有特殊含义。 但是要注意，不遵循此约定会使得你的代码对其他 Python 程序员来说缺乏可读性，    而且也可以想像一个 类浏览器 程序的编写可能会依赖于这样的约定。"""    def __init__(self, name):        self.name = name        self.tricks = []    def add_trick(self, trick):        self.tricks.append(trick)def test_dog2():    d = Dog2('Fido')    e = Dog2('Buddy')    d.add_trick('roll over')    e.add_trick('play dead')    print(d.tricks)class Warehouse:    purpose = 'storage'    region = 'west'def test_warehouse():    """ 如果同样的属性名称同时出现在实例和类中，则属性查找会优先选择实例:    """    w1 = Warehouse()    print(w1.purpose, w1.region)    w2 = Warehouse()    w2.region = 'east'    print(w2.purpose, w2.region, w2)class Person:    passclass Man(Person):    passclass Woman(Person):    passdef test_extend():    man = Man()    print(isinstance(man, Man))    print(isinstance(man, Person))    print(isinstance(man, Woman))    print(issubclass(Man, Person))    print(issubclass(Man, Woman))class Mapping:    """ 那种仅限从一个对象内部访问的“私有”实例变量在 Python 中并不存在。       但是，大多数 Python 代码都遵循这样一个约定：带有一个下划线的名称 (例如 _spam) 应该被当作是 API 的非公有部分        (无论它是函数、方法或是数据成员)。 这应当被视为一个实现细节，可能不经通知即加以改变。       """    """ 那种仅限从一个对象内部访问的“私有”实例变量在 Python 中并不存在。 但是，大多数 Python 代码都遵循这样一个约定：    带有一个下划线的名称 (例如 _spam) 应该被当作是 API 的非公有部分 (无论它是函数、方法或是数据成员)。     这应当被视为一个实现细节，可能不经通知即加以改变。 """    def __init__(self, iterable):        self.items_list = []        self.__update(iterable)    def update(self, iterable):        for item in iterable:            self.items_list.append(item)    __update = update  # private copy of original update() methodclass Reverse:    """Iterator for looping over a sequence backwards."""    def __init__(self, data):        self.data = data        self.index = len(data)    def __iter__(self):        return self    def __next__(self):        if self.index == 0:            raise StopIteration        self.index = self.index - 1        return self.data[self.index]def iter_test():    rev = Reverse('spam')    print(rev)    for s in rev:        print(s)if __name__ == '__main__':    iter_test()
```





## 用法说明

1、http请求批量下载图片信息





## 常见库分析

### requests

#### get请求



#### post请求

其他网络库 urllib 用于网络下载相关的库、beautifulSoup用于格式处理相关的库

#### time



#### numpy



#### beautifulsoup

Pip3 install  bs4



#### pandas

pandas 库中 一维数组 `series`操作

pandas库中多维数组操作`DataFrame`

pandas层次化索引



### 爬虫

web开发可以关注flask、django框架的编写，从事机器学习可以关注tensorflow库的编写



#### Matplotlib

机器学习库

#### seaborn 

机器学习绘图工具



#### 机器学习库tensorflow

#### 





### 源码学习





