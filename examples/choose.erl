[ % choose randomly takes n elements from a list, n=1 by default
	{pool, [{size, 1},
	        {worker_type, dummy_worker}],
		[{print, {sprintf, "~tp and ~tp", [{choose, [777, 333, 111]}, {choose, 2, [9090, 8080, 7070]}]}}]}
].
% should output shomething like this:
% Appending "777 and [9090,8080]", Meta: [{function,print},{line,4},{pool_name,"pool1"},{worker_id,1}]
