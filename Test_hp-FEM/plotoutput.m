dat = load('equationsystem');

u_h1 = dat(:,1);
%u_h2 = dat(5: 14,1);
%u_h3 = dat(15:end,1);


x_h1 = dat(:,2);
%x_h2 = dat(5: 14,2);
%x_h3 = dat(15:end,2);

NE = 6;
%NE = size(u_h, 1);
%x_h(1:NE) = 0;

for i=1:NE
  x_h(i) = (i-1)/(NE-1);
end
x_h = x_h';
%size(x_h,1);
%size(u_h,1);

x(1:(NE*10)) = 0;

for i=1:NE*10
  x(i) = (i-1)/(10*NE-1);
end
x= x';
u(1:10*NE)= sin(2*pi * x);
%% create a graph
f = figure('visible','on');
  set (0, 'defaultaxesfontname', 'Helvetica');
  set (0, 'defaultaxesfontsize', 14);
  set (0, 'defaulttextfontname', 'Helvetica');
  set (0, 'defaulttextfontsize', 14);
  set (gcf, 'Paperunits', 'centimeters');
  set (gcf, 'papersize', [20.98 29.68]);
  set (gcf, 'paperposition', [0.63 0.63 19.72 28.41]) ;

% add multiple plots to the figure
set(gcf, 'nextplot','add');
set(gca, 'nextplot','add');


plot( x_h1,u_h1, 'r','marker','x','MarkerSize',2,'LineWidth',3, ...
      x,u, 'b','marker','x','MarkerSize',2 ,'LineWidth', 3)

grid 'on';

xlabel('t');
ylabel('u');
%legend ('NE = 3', 'NE = 9', 'NE = 27', 'exakt');
print(f, 'output.pdf')
%title('\"Uberschrift
