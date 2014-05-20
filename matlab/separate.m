all_data = load('all.dta');
set_idx  = load('all.idx');

format      = '%u %u %u %u\n';
qual_format = '%u %u %u\n';

non_base     = all_data(find(set_idx ~= 1), :);
non_base_idx = set_idx(find(set_idx ~= 1));

base         = all_data(find(set_idx == 1), :);
valid        = non_base(find(non_base_idx == 2), :);
hidden       = non_base(find(non_base_idx == 3), :);
probe        = non_base(find(non_base_idx == 4), :);
qual         = non_base(find(non_base_idx == 5), 1:3);

% Write .dta files
fprintf(fopen('base.dta', 'w'),   format,      base');
fprintf(fopen('valid.dta', 'w'),  format,      valid');
fprintf(fopen('hidden.dta', 'w'), format,      hidden');
fprintf(fopen('probe.dta', 'w'),  format,      probe');
fprintf(fopen('qual.dta', 'w'),   qual_format, qual');

fclose('all');


% Make sparse matrices.
base_S   = sparse(base(:, 1),   base(:, 2),   base(:, 4));
valid_S  = sparse(valid(:, 1),  valid(:, 2),  valid(:, 4));
hidden_S = sparse(hidden(:, 1), hidden(:, 2), hidden(:, 4));
probe_S  = sparse(probe(:, 1),  probe(:, 2),  probe(:, 4));
qual_S   = sparse(qual(:, 1),   qual(:, 2),   zeros(size(qual, 1), 1));

% Write them.
mmwrite(strcat('no_date/mm/', 'base'),   base_S);
mmwrite(strcat('no_date/mm/', 'valid'),  valid_S);
mmwrite(strcat('no_date/mm/', 'hidden'), hidden_S);
mmwrite(strcat('no_date/mm/', 'probe'),  probe_S);
mmwrite(strcat('no_date/mm/', 'qual'),   qual_S);
