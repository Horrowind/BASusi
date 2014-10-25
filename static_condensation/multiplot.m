% load the result
inp = load('build/input');
outp = load('build/output');

n   = inp(1)
m   = inp(2)
a   = inp(3)
b   = inp(4)

u   = outp(:, :);

x(1:n+1) = 0.;
y(1:m+1) = 0.;
for i = 1:n+1
	  x(i) = ((i-1)*a)/n;
end

for i = 1:m+1
	  y(i) = ((i-1)*b)/m;
end 



% create a graph
h = figure('visible','on');
set (0, 'defaultaxesfontname', 'Helvetica');
set (0, 'defaultaxesfontsize', 14);
set (0, 'defaulttextfontname', 'Helvetica');
set (0, 'defaulttextfontsize', 14);
set (gcf, 'Paperunits', 'centimeters');
set (gcf, 'papersize', [25.98 25.68]);
set (gcf, 'paperposition', [0.68 0.68 24.72 25.41]) ;

% add multiple plots to the figure
set(gcf, 'nextplot','add');
set(gca, 'nextplot','add');


surf(x, y, u)
%  plot(x,u(1,:))

grid 'on';

%title(['Burger equation test case at t = ', num2str(t),' NE: ',num2str(NE),' PO: ', num2str(PO),',Viscosity parameter: ',num2str(c_ny) ]);
%xlabel('x');
%ylabel('Solution u_h');

%legend({'Numerical solution','Normalized entropy viscosity','Normalized entropy residual'});

print(h, 'output.pdf')
