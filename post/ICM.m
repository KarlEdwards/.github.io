% Adapted by KE on May 7, 2018 from https://github.com/samehkhamis/SimplePGM/blob/master/icm.m

% --------
% ICM
% --------

function [y] = icm(fg, y0, tmax)
vars = get_vars(fg);
n = numel(vars);
m = numel(fg);
if ~exist('y0', 'var'), y0 = ceil(rand(1, n) .* [vars.Arity]); end
if ~exist('tmax', 'var'), tmax = 100; end

y = y0;
for t = 1:tmax
    v = ceil(rand() * n);
    phi = ones(1, vars(v).Arity);
    
    for j = 1:numel(vars(v).Factor)
        f = vars(v).Factor(j);
        i = vars(v).Index(j);

        index = num2cell(y(fg(f).Member));
        index{i} = ':';
        phi = phi .* reshape(squeeze(fg(f).P(index{:})), 1, []);
    end
    
    [dummy, index] = max(phi);
    y(v) = index;
end


% --------
% /get_vars.m
% --------

function [vars] = get_vars(fg)
n = max(unique([fg.Member]));
vars = struct('Arity', cell(1, n), 'Factor', cell(1, n), 'Index', cell(1, n));
for i = 1:n
    % Variable factors
    [f, j] = arrayfun(@(f) find(f.Member == i, 1), fg, 'UniformOutput', false);
    vars(i).Factor = find(cellfun(@(x) ~isempty(x), f));
    vars(i).Index = [j{:}];
    
    % Variable arity
    fac = fg(vars(i).Factor(1));
    [dummy, j] = find(fac.Member == i, 1);
    vars(i).Arity = size(fac.P, j);
end
