void main(){
    int target = 9;
    [] int b[10];
    fib(b, target);
    printi(b[9]);
}

int fib([] int f, int t){
    f[0] = 1;
    f[1] = 1;
    for(int i = 2; i <= t; i=i+1)
    {
       f[i] = f[i - 1] + f[i - 2];
    }
}