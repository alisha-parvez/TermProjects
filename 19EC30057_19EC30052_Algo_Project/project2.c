#include <stdio.h>
#include <stdlib.h>
#include <math.h>  

typedef struct line_struct{
    //form of line: ax + b
    double a;
    double b;
} Line;

typedef struct pt{
    double x;
    double y;
} Point;

void print_input(Point point_list[], double *cum_sum[], int n)
{
    printf("Point x y xy x^2\n\n");
    for(int i=1;i<=n;i++)
    {
        printf("(%lf,%lf) ",point_list[i].x,point_list[i].y);
        for(int j=1;j<=4;j++)
            printf("%lf ",cum_sum[j][i]);
        printf("\n");
    }
}

void input(int n, Point *point_list, double *cum_sum[])
{
    for(int i=1;i<=n;i++)
    {
        
        scanf("%lf %lf",&point_list[i].x,&point_list[i].y);

        if(i==0)
        {
            cum_sum[1][i] = 0;
            cum_sum[2][i] = 0;
            cum_sum[3][i] = 0;
            cum_sum[4][i] = 0;
        }

        if(i==1)
        {
            cum_sum[1][i] = point_list[i].x;
            cum_sum[2][i] = point_list[i].y;
            cum_sum[3][i] = (point_list[i].x)*(point_list[i].y);
            cum_sum[4][i] = (point_list[i].x)*(point_list[i].x);
        }
        else
        {
            cum_sum[1][i] = cum_sum[1][i-1] + point_list[i].x;
            cum_sum[2][i] = cum_sum[2][i-1] + point_list[i].y;
            cum_sum[3][i] = cum_sum[3][i-1] + (point_list[i].x)*(point_list[i].y);
            cum_sum[4][i] = cum_sum[4][i-1] + (point_list[i].x)*(point_list[i].x);
        }
    }
}

//finds line through p(i,i+1,...,j) O(1)
Line find_line(Point *point_list, int i, int j, double *cum_sum[])
{
    Line l;
    
    double num = (j-i+1)*(cum_sum[3][j]-cum_sum[3][i-1]) - (cum_sum[1][j]-cum_sum[1][i-1])*(cum_sum[2][j]-cum_sum[2][i-1]);
    double den = (j-i+1)*(cum_sum[4][j]-cum_sum[4][i-1]) - (cum_sum[1][j]-cum_sum[1][i-1])*(cum_sum[1][j]-cum_sum[1][i-1]);
    l.a = num/den;

    num = (cum_sum[2][j]-cum_sum[2][i-1]) - (l.a)*(cum_sum[1][j]-cum_sum[1][i-1]) ;
    l.b = num/(j-i+1);

    return l;
}

double error(Point P[],int st,int end,double a,double b) //error function
{   
    if(st == end)
        return 0;
    
    double error=0.0;
    for(int i=st;i<=end;i++)
    {
        error+=(P[i].y- a*P[i].x - b)*(P[i].y- a*P[i].x - b);
    }
    
    return error;
}

//merges two sorted arrays 
void merge(Point *l1, Point *l2, Point *final_list, int s1, int s2)
{
    int a=1,b=1,c=1;
    while(a<=s1 && b<=s2)  
    {
        if(l1[a].x < l2[b].x)  // change condition acc to need !!!
            final_list[c++] = l1[a++];
        else
            final_list[c++] = l2[b++];
    }

    while(a<=s1)
        final_list[c++] = l1[a++];

    while(b<=s2)
        final_list[c++] = l2[b++];
}

//sorts an array of points using merge sort
void sort(Point pt_list[], int n)
{
    if(n==1)
        return;
    
    int s1 = n/2;

    Point *l1 = (Point *)malloc((s1+1)*sizeof(Point));
    Point *l2 = (Point *)malloc((n-s1+1)*sizeof(Point));

    int j=1;
    for(int i=1;i<=s1;i++,j++)
        l1[i] = pt_list[j];

    for(int i=1;i<=n-s1;i++,j++)
        l2[i] = pt_list[j];

    sort(l1, s1);
    sort(l2,n-s1);

    merge(l1,l2,pt_list,s1,n-s1);

    free(l1);
    free(l2);
}

double **find_error_matrix(Point *point_list, double *cum_sum[], int n)
{
    double **err = (double **)malloc((n+1)*sizeof(double *)); 
    for (int i=1; i<=n; i++) 
        err[i] = (double *)malloc((n+1) * sizeof(double));
    
    Line temp;

    for(int i = 1; i<= n; i++)
    {
        for(int j=i; j<=n;j++)
        {
            if(i==j)
            {
                err[i][j] = 0;
                continue;
            }

            temp = find_line(point_list, i,j,cum_sum);
            err[i][j] = error(point_list, i,j,temp.a,temp.b);
        }
    }

    return err;
}

void find_optimal(int j, double **err, double OPT[], double c)
{
    if(j == 0)
    {
        OPT[0] = 0;
        return;
    }
    
    int min_i = 1;
    double min_cost = err[1][j] + c + OPT[0];
    double temp;

    for(int i=2;i<=j;i++)
    {
        if(OPT[i-1] == -1)
            find_optimal(i-1,err,OPT,c);
        
        temp = err[i][j] + c + OPT[i-1];
        if(temp < min_cost)
        {
            min_cost = temp;
            min_i = i;
        }
    }

    OPT[j] = min_cost;
}

void find_segments(int j, Point *point_list, double *cum_sum[], double **err, double OPT[], double c)
{
    if(j == 0)
        return;
    
    int min_i = 1;
    double min_cost = err[1][j] + c + OPT[0];
    double temp;

    for(int i=2;i<j;i++)
    {
        temp = err[i][j] + c + OPT[i-1];
        if(temp < min_cost)
        {
            min_cost = temp;
            min_i = i;
        }
    }

    Line l = find_line(point_list,min_i,j,cum_sum);

    if(!isnan(l.a) && !isnan(l.b))
        printf("(%lf)x + (%lf)     for (%.2lf,%.2lf) to (%.2lf,%.2lf)\n",l.a, l.b, point_list[min_i].x,point_list[min_i].y,point_list[j].x,point_list[j].y );


    find_segments(min_i-1, point_list, cum_sum, err, OPT,c);
}

int main()
{
    int n;
    double c;
    scanf("%d",&n);
    scanf("%lf",&c);

    //variable creation
    Point *point_list = (Point *)malloc((n+1)*sizeof(Point));
    
    double *cum_sum[5]; 
    for (int i=1; i<=4; i++) 
        cum_sum[i] = (double *)malloc((n+1) * sizeof(double));

    //variable creation ends
   
    input(n, point_list, cum_sum);
    sort(point_list, n);

    double **err = find_error_matrix(point_list,cum_sum,n);
    double *OPT = (double *)malloc((n+1)*sizeof(double));
    OPT[0] = 0;

    for(int i=1;i<=n;i++)
    {
        OPT[i] = -1;
    }

    find_optimal(n, err, OPT,c);

    printf("\nThe minimum cost is: %lf\n\n", OPT[n]);

    find_segments(n,point_list,cum_sum,err, OPT,c);

    free(point_list);
    free(OPT);
    free(err);
}