[ % iterator makes something like for-loop statement
    {pool, [{size, 3},
         {worker_type, dummy_worker}],
        [{loop, [{time, {5, sec}},
                 {rate, {1, rps}},
                 {iterator, "i"}],
            [{print, {sprintf, "~tp", [{var, "i"}]}}]}]}
].

% should output something like this:
% 18:03:17.571 [info] <0.177.0> [ director ] Started all pools
% 18:03:17.573 [info] <0.190.0> Appending "0", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,1}]
% 18:03:17.573 [info] <0.192.0> Appending "0", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,3}]
% 18:03:17.573 [info] <0.191.0> Appending "0", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,2}]
% 18:03:18.573 [info] <0.191.0> Appending "1", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,2}]
% 18:03:18.573 [info] <0.190.0> Appending "1", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,1}]
% 18:03:18.573 [info] <0.192.0> Appending "1", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,3}]
% 18:03:18.573 [info] <0.191.0> Appending "2", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,2}]
% 18:03:18.573 [info] <0.190.0> Appending "2", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,1}]
% 18:03:18.573 [info] <0.192.0> Appending "2", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,3}]
% 18:03:20.573 [info] <0.192.0> Appending "3", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,3}]
% 18:03:20.573 [info] <0.190.0> Appending "3", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,1}]
% 18:03:20.573 [info] <0.191.0> Appending "3", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,2}]
% 18:03:21.574 [info] <0.190.0> Appending "4", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,1}]
% 18:03:21.574 [info] <0.191.0> Appending "4", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,2}]
% 18:03:21.574 [info] <0.192.0> Appending "4", Meta: [{function,print},{line,7},{pool_name,"pool1"},{worker_id,3}]
% 18:03:22.573 [info] <0.186.0> [ "pool1" ] All workers have finished
