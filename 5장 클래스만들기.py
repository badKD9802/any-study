##계산기 만들기(5장)

class FourCal:
    def setdata(self, first, second):
        self.first = first
        self.second = second
    def add(self):
        result = self.first + self.second
        return result
    def mul(self):
        result = self.first*self.second
        return result
    def sub(self):
        result = self.first - self.second
        return result
    def div(self):
        result = self.first/self.second
        return result

a = FourCal()
b = FourCal()
a.setdata(4,2)
b.setdata(3,8)
a.add()
b.add()
a.mul()
b.mul()
a.sub()
b.div()

print(a.add())
print(b.add())
print(b.div())
print(a.sub())
print(a.mul())
print(b.mul())

## 클래스 상속하기

class MoreFourCal(FourCal):
    def pow(self):
        result = self.first**self.second
        return result

class SafeFourcal(ForuCal):
    def div(self):
        if self.second ==0:
            return 0
        else:
            return self.frist / self,second

