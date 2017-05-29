		<!-- LATTICES -->
		<div id="lattices" class="container">
            <form action = "lattices" id = "lattices-form" method="post">
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
                    <div class = "twocols" id = "controls">
                        <div class = "file-controls">
                            <label for="lattice-name">File:</label>
                            <input type = "text" id = "lattice-name" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false" value = "alfabeta.lat">
                            <input type = "file" id="latfile" onchange = upload_lattice_button("textarea-lattice","lattice-name","latfile")>
                            <button type = "button" class = "button-lattice" title = "Upload lattice" onclick = "latfile.click()"><img src = "content/img/lattices/upload.png"></button>
                            <button type = "button" class = "button-lattice" title = "Download lattice" onclick = save_lattice("textarea-lattice","lattice-name")> <img src = "content/img/lattices/download.png"> </button>
                        </div>

                        <div class = "lattice-controls">
                            <button type = "button" class = "button-lattice" title = "Normalize" onclick = normalize_from_text("textarea-lattice")> <img src = "content/img/lattices/normalize.png"> </button>
                            <button type = "button" class = "button-lattice" title = "Add new member" onclick = addMember("textarea-lattice")> <img src = "content/img/lattices/add_member.png"> </button>
                            <button type = "button" class = "button-lattice" title = "Remove member" onclick = removeMember("canvas-lattice","textarea-lattice")> <img src = "content/img/lattices/remove_member.png"> </button>
                            <button type = "button" class = "button-lattice" title = "Add connection" onclick = createArc("canvas-lattice","textarea-lattice")> <img src = "content/img/lattices/add_arrow.png"> </button>
                            <button type = "button" class = "button-lattice" title = "Remove connection" onclick = removeArc("canvas-lattice","textarea-lattice")> <img src = "content/img/lattices/remove_arrow.png"> </button>
                        </div>
                    </div>
                    <br><br>
                    <h2>Properties</h2>
                    <div id = "properties">
                            <label for="category-combo">Category:</label>
                            <select id = "category-combo" class="lattice-selection"> 
                                <option>Basic</option>
                                <option>Combined</option>
                                <option>Multiple</option>
                            </select>
                            <label for="property-combo">Property:</label>
                            <select name = "prop" id = "property-combo" class="lattice-selection"> 
                            </select>
                            <label for="aggr-combo1">Connective 1:</label>
                            <select name = "aggr1" id = "aggr_combo1" class="lattice-selection"> </select>
                            <label for="aggr-combo2">Connective 2:</label>
                            <select name = "aggr2" id = "aggr_combo2" disabled = true class="lattice-selection"> </select>
                            <br><br>
                            <div class = "twocols">
                                <div style="width:80%">
                                    <label for  = "math-definition">Mathematical definition:</label>
                                    <textarea id = "math-definition" readonly  = "readonly" ></textarea>
                                </div>
                                <div style="width:20%">
                                    <input type="submit" id = "test-submit" class = "button-lattice" value = "Test" style = "width:150px;height:50px;margin-top:25px;">
                                </div>
                            </div>
                            <h3>Result</h3><textarea id="property-result" class="code-try" readonly = "readonly" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false" style="height: 200px;"> </textarea>
                    </div>
                </div>
            </form>
		</div>
		<script type="text/javascript">latticeMaker( "textarea-lattice","canvas-lattice" ); properties_tester("textarea-lattice","category-combo","property-combo","aggr_combo1","aggr_combo2","math-definition"); upload_lattice_drop("textarea-lattice","lattice-name"); 
            </script>
