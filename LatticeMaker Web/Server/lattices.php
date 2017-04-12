		<!-- LATTICES -->
		<div id="lattices" class="container">
			<div class="match-width">
				<h1>Lattice Maker</h1>
				<div class="twocols">
					<div><div>
						<h2>Code</h2>
					</div></div>
					<div><div>
						<h2>Graph</h2>
					</div></div>
				</div>
				<div class="twocols">
					<div><div>
						<textarea id="textarea-lattice" name="lat" class="code-try" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false" style="height: 500px;">% Elements
member(alpha).
member(beta).
member(bottom).
member(delta).
member(epsilon).
member(gamma).
member(lambda).
member(tau).
member(top).
members([bottom,alpha,beta,top,delta,gamma,epsilon,tau,lambda]).

% Supremum and infimum
top(top).
bot(bottom).

% Arcs
arc(alpha, tau).
arc(beta, delta).
arc(bottom, alpha).
arc(bottom, beta).
arc(bottom, gamma).
arc(delta, top).
arc(lambda, top).
arc(tau, top).

% Ordering relation
leq(X, X).
leq(X, Y) :- arc(X, Z), leq(Z, Y).

% Binary operations
and_godel(X,Y,Z) :- pri_inf(X,Y,Z).

% Auxiliar operations
pri_inf(bottom,X,bottom) :- !.
pri_inf(alpha,X,alpha) :- leq(alpha,X), !.
pri_inf(beta,X,beta) :- leq(beta,X), !.
pri_inf(top,X,X) :- !.
pri_inf(X,Y,bottom).</textarea>
					</div></div>
					<div><div>
						<canvas id="canvas-lattice"></canvas>
					</div></div>
				</div>
				<div id="lattices-control">
					<input type="button" class="button-lattice" value="Add member" onclick=addMember("textarea-lattice")>
					<input type="button" class="button-lattice" value="Remove member" onclick=removeMember("canvas-lattice","textarea-lattice")>
					<input type="button" class="button-lattice" value="Add connection" onclick=createArc("canvas-lattice","textarea-lattice")>
					<input type="button" class="button-lattice" value="Remove connection">
				</div>
				<h2>Properties</h2>
				<h2>Export lattice</h2>
			</div>
		</div>
		<script type="text/javascript">latticeMaker( "textarea-lattice","canvas-lattice" );</script>
