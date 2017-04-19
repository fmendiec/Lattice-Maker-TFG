/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

        This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(lattice_viewer,
          [ lattice_viewer/0
          ]).

:- use_module(library(pce)).
:- use_module(library(dragdrop)).
:- use_module(library(keybinding)).
:- use_module(library(help_message)).

:- consult('DragMenu.pl').
:- consult('LatticeConnect.pl').
:- consult('LatticeEval.pl').
:- consult('LatticeFind.pl').
:- consult('LatticeLayout.pl').
:- consult('LatticeNode.pl').
:- consult('LatticePrint.pl').
:- consult('LatticeTest.pl').
:- consult('LatticeXml.pl').

:- pce_autoload(tool_bar, library(toolbar)).
:- pce_autoload(finder, library(find_file)).
:- pce_autoload(helper, library(pce_helper)).
:- pce_autoload(prolog_predicate, library(prolog_predicate)).

:- pce_global(@finder, new(finder)).
:- pce_global(@helper, new(helper)).
:- pce_help_file(latticehelp, 'index.xml').

:- dynamic lat_graph:member/1.
:- dynamic lat_graph:members/1.
:- dynamic lat_graph:top/1.
:- dynamic lat_graph:bot/1.
:- dynamic lat_graph:arc/2.
:- dynamic lat_graph:leq/2.
:- dynamic lat_graph:level/2.
:- dynamic lat_graph:distance/3.
:- dynamic lat_graph:fromNode/1.

:- pce_image_directory('.\\bitmaps').

resource(imgnew, image, image('new.xpm')).
resource(imgopen, image, image('open.xpm')).
resource(imgsave, image, image('save.xpm')).
resource(imgquit, image, image('quit.xpm')).
resource(imgimage, image, image('image.xpm')).
resource(imgxml, image, image('xml.xpm')).
resource(imgprint, image, image('print.xpm')).
resource(imgclear, image, image('clear.xpm')).
resource(imgedit, image, image('edit.xpm')).
resource(imgadd, image, image('add.xpm')).
resource(imgclean, image, image('clean.xpm')).
resource(imgfind, image, image('find.xpm')).
resource(imggraph, image, image('complete.xpm')).
resource(imgnode, image, image('node.xpm')).
resource(imgarrow, image, image('arrow.xpm')).
resource(imgnormalize, image, image('normalize.xpm')).
resource(imgsendup, image, image('sendup.xpm')).
resource(imgundo, image, image('undo.xpm')).
resource(imgeditfind, image, image('find_replace.xpm')).
resource(imghelp, image, image('help.xpm')).

lattice_viewer :-
	new(@GV, lattice_viewer),
    send(@GV, open_centered).

:- pce_begin_class(lattice_viewer, frame).

variable(drawnode, bool, both, "Node selected").
variable(drawarrow,     bool, both, "Arrow selected").
variable(new, bool, both, "New Graph").
variable(modified, bool, both, "Graph modified").
variable(editable, bool, both, "Editor editable").
variable(filename, name, both, "File loaded").
variable(redrawn, bool, both, "Graph redrawn").
variable(max_nodes_layer, int, both, "Nodes in widest layer").

% Local configuration
locale_create(_, default, [alias(lattice), decimal_point('.'), thousand_sep(''), grouping([repeat(3)])]).
set_locale(lattice).

initialise(GV) :->
	"Create lattice_viewer"::
	send(GV, send_super, initialise, 'Lattice Maker'),
	send(GV, background, gainsboro),

	send(GV, append, new(P, picture)),
	send(P, size, size(80, 350)),
	send(P, label, 'Graphical representation of lattice'),

	send(new(DD, dialog(edit_toolbar)), below, P),
	send(DD, size, size(80, 25)),
	send(DD, background, gainsboro),
	send(DD, pen, 0),
	send(DD, append, new(TB2, tool_bar)),
	send(TB2, colour, gainsboro),
	send(TB2, gap, size(3, 3)),
	fill_edit_toolbar(TB2),
	
	send(new(V, view), below, P),
	send(V, font, font(arial, normal, 12)),
	send(V, size, size(80, 15)),
    send(V, editable, @off),
	send(V, label, 'Aggregator definitions in lattice'),

	new(Pm, message(GV, newnode, @event?position)),
	send(P, recogniser, click_gesture(left, '', single,
			  message(Pm, execute))),
	send(V, recogniser, popup_gesture(new(Pop, popup))),
	send_list(Pop, append,
			  [ menu_item('undo       Ctrl+Z', message(@arg1?editor, undo),
												condition := message(GV, get_edit_mode)),
				menu_item('copy       Ctrl+C', message(@arg1?editor, copy),
												condition := message(GV, get_edit_mode)),
				menu_item('paste      Ctrl+V', message(@arg1?editor, paste),
												condition := message(GV, get_edit_mode)),
				menu_item('cut         Ctrl+X', message(@arg1?editor, cut),
												condition := message(GV, get_edit_mode)),
				menu_item('find        Ctrl+F', message(GV, find_in),
												condition := message(GV, get_edit_mode)),
				menu_item('select_all Ctrl+A', message(@arg1?editor, mark_whole_buffer), end_group := @on,
												condition := message(GV, get_edit_mode)),
				menu_item(toggle_edit_mode, message(@arg1, toggle_edit_mode), end_group := @on),
				menu_item(show_key_bindings, message(@prolog, show_key_bindings, @arg1))
				]),

	send(V, key_binding, '\\C-f', message(GV, find_in)),
	send(V, key_binding, '\\C-a', message(V?editor, mark_whole_buffer)),
	send(new(Oper, dialog), right, V),
	send(Oper, name, dialog_eval),
	send(Oper, background, gainsboro),
	fill_operators_dialog(Oper),

	send(new(R, dialog(report)), below, V),
	send(R, background, gainsboro),
	fill_dialog(R),

	send(new(Menu, dialog(dialog_menu)), above, P),
	% #D8D8D8 = gainsboro
	send(Menu, background, gainsboro),
	send(Menu, pen, 0),
	send(Menu, gap, size(3, 3)),
	send(Menu, append, new(MB, menu_bar)),
	fill_menu(MB),
	send(Menu, append, new(TB, tool_bar)),
	send(TB, colour, gainsboro),
	send(TB, gap, size(3, 3)),
	fill_toolbar(TB),

	send(GV, icon, image('image.xpm')),
	send(GV, fit),
	send(GV, set_filename, ''),
	send(GV, set_initial_state).

fill_menu(MB) :-
	get(MB, frame, FrameMB),
	get(FrameMB, member, picture, P),

	send(MB, append, new(F, popup(file))),
	send(MB, append, new(E, popup(edit))),
	send(MB, append, new(G, popup(graphic))),
	send(MB, append, new(A, popup(aggregator))),
	send(MB, append, new(H, popup(help))),

	send(F, append, menu_item(new_lattice, message(FrameMB, new))),
	send(F, append, menu_item(load_lattice, message(FrameMB, load))),
	send(F, append, menu_item(save_to_file, message(FrameMB, save),
									condition := message(FrameMB, lattice_noempty))),
	send(F, append, menu_item(save_as, message(FrameMB, saveAs),
									condition := message(FrameMB, lattice_noempty))),
	send(F, append, menu_item(print, message(FrameMB, print),
									condition := message(FrameMB, graph_noempty, P),
									end_group := @on)),
	send(F, append, menu_item(export_image, message(FrameMB, image),
									condition := message(FrameMB, graph_noempty, P))),
	send(F, append, menu_item(export_lattice_to_XML, message(FrameMB, export_xml),
									condition := message(FrameMB, lattice_noempty),
									end_group := @on)),
	send(F, append, menu_item(exit_program, message(FrameMB, bye))),

	send(E, append, menu_item(clear_text_editor, message(FrameMB, clear_editor),
									condition := message(FrameMB, get_edit_mode))),
	send(E, append, menu_item(toggle_edit_mode, message(FrameMB, toggle_edit_mode))),
	send(E, append, menu_item(add_connective, message(FrameMB, add_connective),
									condition := message(FrameMB, get_edit_mode))),
	send(E, append, menu_item(find_and_replace, message(FrameMB, find_in),
									condition := message(FrameMB, get_edit_mode),
									end_group := @on)),
	send(E, append, menu_item(redraw_graph_from_lattice, message(FrameMB, lattice_to_graph),
									condition := message(FrameMB, lattice_noempty))),
	send(E, append, menu_item(undo_redraw_graph, message(FrameMB, undo_send_graph),
									condition := message(FrameMB, lattice_noempty))),

	send(G, append, menu_item(clear_graph_editor, message(FrameMB, clear_graph),
									condition := message(FrameMB, graph_noempty, P),
									end_group := @on)),
	send(G, append, menu_item(set_truth_degree_mode, message(FrameMB, set_selected_node))),
	send(G, append, menu_item(set_leq_arrow_mode, message(FrameMB, set_selected_arrow),
									end_group := @on)),
	send(G, append, menu_item(find_truth_degree, message(FrameMB, find),
									condition := message(FrameMB, graph_noempty, P))),
	send(G, append, menu_item(complete_graph, message(FrameMB, complete),
									condition := message(FrameMB, graph_noempty, P))),
	send(G, append, menu_item(normalize_graph, message(FrameMB, normalize),
									condition := message(FrameMB, graph_noempty, P))),

	send(A, append, menu_item(evaluate, message(FrameMB, eval_selected_aggregator),
									condition := message(FrameMB, lattice_noempty))),
	send(A, append, menu_item(test_aggregator, message(FrameMB, test_selected_aggregator),
									condition := message(FrameMB, lattice_noempty))),
    send(A, append, menu_item(distance, message(FrameMB, eval_distance),
									condition := message(FrameMB, lattice_noempty))),
    
	send_list(H, append, [ menu_item(user_manual, message(@helper, give_help, latticehelp, 'latticehelp')),
						   menu_item(about, message(FrameMB, about_dialog))
						   ]).

about_dialog(F) :->
	new(ND, dialog('About: LatticeMaker')),
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),
	add_label(ND, about1, 'LatticeMaker v1.0', bold, blue, 14),
	add_label(ND, about2, 'University of Castilla-La Mancha (Spain)', normal, black, 12),
	add_label(ND, about3, 'SWI-Prolog and XPCE Graphical Interface for Lattice Edition',
							normal, black, 12),
	add_label(ND, about3, 'by Maria del Señor Martinez Ruiz', normal, black, 12),
	add_label(ND, about3, 'mds.martinezruiz@gmail.com', normal, black, 12),
	send(ND, append, button(close, message(ND, destroy))),
	send(ND, open).

fill_toolbar(TB) :-
	get(TB, frame, FrameTB),

	send(TB, append, tool_button(message(FrameTB, new), resource(imgnew), new_lattice)),
	send(TB, append, tool_button(message(FrameTB, load), resource(imgopen), load_lattice)),
	send(TB, append, tool_button(message(FrameTB, save), resource(imgsave), save_to_file)),
	send(TB, append, tool_button(message(FrameTB, print), resource(imgprint), print)),
	send(TB, append, tool_button(message(FrameTB, image), resource(imgimage), export_image)),
	send(TB, append, tool_button(message(FrameTB, export_xml), resource(imgxml), export_to_xml)),
	send(TB, append, gap),
	send(TB, append, tool_button(message(FrameTB, clear_graph), resource(imgclear), clear_graph_editor)),
	send(TB, append, tool_button(message(FrameTB, set_selected_node), resource(imgnode), set_truth_degree_mode)),
	send(TB, append, tool_button(message(FrameTB, set_selected_arrow), resource(imgarrow), set_leq_arrow_mode)),
	send(TB, append, tool_button(message(FrameTB, find), resource(imgfind), find_truth_degree)),
	send(TB, append, tool_button(message(FrameTB, complete), resource(imggraph), complete)),
	send(TB, append, tool_button(message(FrameTB, normalize), resource(imgnormalize), redraw_normalize_graph)),
	send(TB, append, gap),
	send(TB, append, tool_button(message(@helper, give_help, latticehelp, 'latticehelp'), resource(imghelp), help)),
	send(TB, append, gap),
	send(TB, append, tool_button(message(FrameTB, bye), resource(imgquit), exit_program)).

fill_edit_toolbar(TB) :-
	get(TB, frame, FrameTB),
	send(TB, append, tool_button(message(FrameTB, clear_editor), resource(imgclean), clear_text_editor)),
	send(TB, append, tool_button(message(FrameTB, toggle_edit_mode), resource(imgedit), toggle_edit_mode)),
	send(TB, append, tool_button(message(FrameTB, add_connective), resource(imgadd), add_connective)),
	send(TB, append, tool_button(message(FrameTB, find_in), resource(imgeditfind), find_and_replace)),
	send(TB, append, gap),
	send(TB, append, tool_button(message(FrameTB, lattice_to_graph), resource(imgsendup), redraw_graph_from_lattice)),
	send(TB, append, tool_button(message(FrameTB, undo_send_graph), resource(imgundo), undo_redraw_graph)).

fill_operators_dialog(D) :-
	add_label(D, aggrhelp, 'Select an Aggregator to EVAL or TEST', normal, blue, 12),
    % Aggregator Control
	send(D, append, new(B, menu(aggregators, cycle, message(D, fill_terms)))),
	send_list(B, append, [empty]),
	send(B, alignment, left),
    % Second Aggregator Control
	send(D, append, new(S, menu(second, cycle, message(D, fill_terms))),right),
	send_list(S, append, [empty]),
	send(S, alignment, center),
    send(S, show_label, @on),
	send(S, active, @off),
    
    % Properties definition box
    send(D,append,new(PD,text_item(definition))),
    send(PD,editable,@off),

	send(D, append, new(W1, dialog_group(group1, group))),
	send(W1, alignment, center),
	send(W1, append, new(W2, dialog_group(group11, group))),
	send(new(W3, dialog_group(group12, group)), right, W2),

	add_label(W2, infor1, 'Select the Terms to EVAL', normal, blue, 12),
	add_label(W2, infor2, '(Shift + Drag and Drop can be used)', normal, blue, 12),
	add_dragmenu(W2, 1, 6),

	add_label(W3, infor1, 'Select the property to TEST', normal, blue, 12),
    add_label(W3, infor2, '(Some need two aggregators)', normal, blue, 12),
	send(W3, append, new(E, menu(evaluation, cycle, message(D, options, @arg1)))),
	send(E, show_label, @off),
	send_list(E, append, [noSelection,frontier_top, frontier_bot, increasing, non_increasing, decreasing, non_decreasing]),
	send_list(E, append, [switchness, adjointness, monotone,reflexivity,commutativity, distributivity]),
	send(E, layout, vertical),
	send(E, alignment, center),
    
    add_label(W3, infor1, '\nDistance checker', normal, blue, 12),
    add_label(W3, infor2, '(Shift + Drag and Drop can be used)', normal, blue, 12),
    add_dragmenu(W3,1,2),
    
	send(D, gap, size(40, 10)),
	new(BEval, button(eval, message(D, eval_selected_aggregator))),
	send(D, append, BEval),
	send(BEval, font, font(arial, bold, 12)),
	send(BEval, colour, blue),
    send(BEval, help_message, tag, 'Evaluate the aggregator and terms selected'),

	new(Output, view),
	send(D, append, new(BClear, button(clear, message(Output, clear))), right),
	send(BClear, font, font(arial, bold, 12)),
	send(BClear, colour, blue),
    send(BClear, help_message, tag, 'Clear the results view'),

	send(D, append, new(BTest, button(test, message(D, test_selected_aggregator))), right),
	send(BTest, font, font(arial, bold, 12)),
	send(BTest, colour, blue),
    send(BTest, help_message, tag, 'Test the property selected'),

    send(D, append, new(BDist, button(distance, message(D, eval_distance))), right),
	send(BDist, font, font(arial, bold, 12)),
	send(BDist, colour, blue),
    send(BDist, help_message, tag, 'Check the distances'),
    
	send(D, append, Output, next_row),
	send(Output, size, size(50, 8)),
	send(Output, alignment, center),
	send(Output, editable, @off),

	send(D, resize_message, message(D, layout, @arg2)).

options(F, Opt) :->
	get(F, member(dialog_eval), D),
    % Unlock the Second Aggregator Combobox
	get(D, member, second, SC),
    (       (Opt == switchness ; Opt == distributivity)
    ->      send(SC, active, @on)
    ;       send(SC, active, @off)
    ),
    
    get(D,member,definition,TP),
    send(TP,clear),
    get_aggregator(F,D,aggregators,Name,_),
    (   Name == '' ->
                     ((Opt == switchness ; Opt == distributivity) ->  get_math_def(Opt,'$1','$2',Exp)  ;  get_math_def(Opt,'$',Exp))
                     ; ((Opt == switchness ; Opt == distributivity) -> get_aggregator(F,D,second,Name2,_),get_math_def(Opt,Name,Name2,Exp) ; get_math_def(Opt,Name,Exp))
    ),
    send(TP,append,Exp).
    
get_math_def(frontier_top,X,Z) :- format(atom(Z),"~w( T, T ) = T",[X]).
get_math_def(frontier_bot,X,Z) :- format(atom(Z),"~w( B, B ) = B",[X]).
get_math_def(increasing,X,Z) :- format(atom(Z),"If  X  <  Y  =>  ~w ( X, Y )  <  ~w ( Y, Z )",[X,X]).
get_math_def(non_decreasing,X,Z) :- format(atom(Z),"If  X  <  Y  =>  ~w ( X, Z )  =<  ~w ( Y, Z )",[X,X]).
get_math_def(decreasing,X,Z) :- format(atom(Z),"If  X  <  Y  =>  ~w ( X, Z )  >  ~w ( Y, Z )",[X,X]).
get_math_def(non_increasing,X,Z) :- format(atom(Z),"If  X  <  Y  =>  ~w ( X, Z )  >=  ~w ( Y, Z )",[X,X]).
get_math_def(reflexivity,X,Z) :- format(atom(Z),"~w( X, X ) = X",[X]).
get_math_def(commutativity,X,Z) :- format(atom(Z),"~w ( X, Y ) == ~w ( Y, X )",[X,X]).
get_math_def(switchness,X,Y,Z) :- format(atom(Z),"~w ( ~w ( X, Y ), Z ) == ~w ( X, ~w ( Y, Z ) )",[X,Y,Y,X]).
get_math_def(distributivity,X,Y,Z) :- format(atom(Z), "~w ( ~w ( X, Y ) , Z ) == ~w ( ~w ( X, Z) , ~w ( Y, Z ) )",[Y,X,X,Y,Y]).


fill_terms(F) :->
	get(F, member(dialog_eval), D),
	get_container_combo(D, C),
	get(D, member, aggregators, Aggr),
	get(Aggr, selection, AggrSelection),
	(   AggrSelection == empty
	->  send(F, report, error, 'EMPTY list of aggregators. Load a lattice and retry.')
	;   (	atomic_list_concat([_, Arity], '/', AggrSelection),
			atom_number(Arity, NumA),
			activate_dragmenu(C, 1, 6, @off),
			activate_dragmenu(C, 1, NumA, @on)
		)
	).

add_label(D, Name, Text, Style, Colour, Size) :-
	send(D, append, new(L, label(Name, Text))),
	send(L, alignment, center),
	send(L, length, 0),
	send(L, colour, Colour),
	send(L, font, font(arial, Style, Size)).

add_dragmenu(G, DefArity, Arity) :-
	forall(between(DefArity, Arity, I),
			(       get_term_name(I, StrText),
					get_var_name(I, Var),
					send(G, append, new(M, dragmenu(StrText))),
					send(M, alignment, center),
					send(M, displayed, @on),
					send(M, append, Var)
			)).
fill_dragmenu(G, DefArity, Arity, ListAdd) :-
	forall(between(DefArity, Arity, I),
			(       get_term_name(I, StrText),
					get_var_name(I, Var),
					get(G, member(StrText), DM),
					send(DM, clear),
					send(DM, append, Var),
					(   ListAdd == []
					->  true
					;   send_list(DM, add_member, ListAdd)
					)
			)).
			
get_term_name(I, StrText):-
	name(I, StrI),
	append("term_", StrI, Text),
	name(StrText, Text).
get_var_name(I, StrText):-
	name(I, StrI),
	append("x", StrI, Text),
	name(StrText, Text).
	
activate_dragmenu(G, DefArity, Arity, OnOff) :-
	forall(between(DefArity, Arity, I),
			(       get_term_name(I, StrText),
					get(G, member(StrText), DM),
					send(DM, displayed, OnOff)
			)).

fill_dialog(D) :-
	add_label(D, reporter, '', bold, red, 12).

new(F) :->
	ok_changes(F),
	send(F, clearspace),
	lat_graph:assert(lat_graph:leq(X, X)),
	lat_graph:assert(':-'(lat_graph:leq(X, Y), (lat_graph:arc(X, Z), lat_graph:leq(Z, Y)))),
	send(F, set_new_state),
	send(F, set_filename, ''),
	send(F, set_selected_node).

load(F) :->
	"Ask for a file name"::
	ok_changes(F),
	get(F, member, view, V),
	get(@finder, file, @on, '.lat', FileName),

	send(F, clearspace),
	(	wipe_module(lat_graph),
		lat_graph:consult(FileName)
	->	(	validate_lattice(lat_graph)
		->	send(V, load, FileName),
			send(F, report, status, 'File %s read', FileName),
			send(F, label, string('LatticeMaker - %s', FileName)),		
			% "Show file content"
			send(F, set_filename, FileName),
			send(F, set_nonew_state),
			send(F, normalize)
		;	send(F, report, error, 'File %s hasn\'t a lattice format', FileName)
		)
	;	send(F, report, error, 'File %s has errors', FileName)
	).
	
save(F) :->
	"Write to file"::
	get_new_state(F)
	->	send(F, saveAs)
	;	send(F, lattice_noempty),
		ok_normalize(F),
		(	get_filename(F, FileName)
		->	true
		;	get(@finder, file, save, '.lat', FileName)
		),
		add_generated_to_file(F, FileName, Path),

		get(F, member, view, V),
		send(V, save, Path),

		send(F, report, status, 'Saved in %s', Path),
		send(F, label, string('LatticeMaker - %s', Path)),		
		send(F, set_nomodified_state),
		send(F, set_filename, Path).

add_generated_to_file(F, FileName, Path) :-
	file_base_name(FileName, JustName),
	file_directory_name(FileName, DirName),
	(	atom_concat('generated_', _, JustName)
	->  Path = FileName
	;   atom_concat('generated_', JustName, NewJustName),
		directory_file_path(DirName, NewJustName, Path)
	),
	(   exists_file(Path)
	->  get(F, msgbox, '"generated_" file already exists. Rewrite it?', RT),
		RT \= @nil
	;   true
	).

saveAs(F) :->
	"Write to file"::
	writeln('entro en saveas'),
	send(F, lattice_noempty),
	ok_normalize(F),
	get(@finder, file, save, '.lat', FileName),
	add_generated_to_file(F, FileName, Path),
	
	get(F, member, view, V),
	send(V, save, Path),

	send(F, report, status, 'Saved in %s', Path),
	send(F, label, string('LatticeMaker - %s', Path)),		
	send(F, set_nomodified_state),
	send(F, set_filename, Path).

image(F):->
	get(F, member, picture, P),
	send(F, graph_noempty, P),

	new(ND, dialog('Image file format')),
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),

	send(ND, append, new(L, label(infor, 'Select one of this image formats'))),
	send(L, alignment, center),
	send(L, length, 0),
	send(L, colour, blue),
	send(L, font, font(arial, normal, 12)),
	new(CB, menu(image, cycle)),
	send(ND, append, CB),
	send_list(CB, append, [xpm,jpeg,gif]),
	send(CB, alignment, center),
	send(ND, append, button(ok, message(ND, return, CB?selection))),
	send(ND, append, button(cancel, message(ND, return, @nil))),
	send(ND, default_button, ok),
	get(ND, confirm_centered,
						F?area?center, ImgFormat),
	send(ND, destroy),
	ImgFormat \== @nil,

	get_name_for_file(F, ImgFormat, Path),
 	new(File, file(Path)),
	send(File, open, write),

	picture_to_image(P, Image),

	send(Image, save, File, ImgFormat),
	send(File, close),
	send(File, done),
	send(F, report, status, 'Saved Image in %s', Path),
	send(F, layout).

picture_to_image(P, I) :-
	get(P, bounding_box, area(X, Y, W, H)),
	new(I, image(@nil, W, H, pixmap)),
	new(M1, point(-X, -Y)),
	send(P?graphicals, for_all, and(message(@arg1, relative_move, M1),
									message(I, draw_in, @arg1))).

export_xml(F) :->
	"Export to XML file"::
	send(F, lattice_noempty),
	ok_normalize(F),
	get_name_for_file(F, xml, Path),

	new(ND, dialog('Lattice XML document')),
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),

	add_label(ND, label1, 'This is the XML document for current lattice', bold, dark_blue, 12),
	add_label(ND, label2, 'Do you want to save it?', bold, dark_blue, 12),
	send(ND, append, new(NV, view(text))),

	send(ND, append, button(ok, and(message(NV, save, Path),
									message(ND, return, ok)))),
	send(ND, append, button(cancel, message(ND, return, @nil))),
	send(ND, default_button, ok),

	new(Buffer, text_buffer),
	generate_XML(F, Buffer),

	send(NV, text_buffer, Buffer),
	send(NV, editable, @off),

	get(ND, confirm_centered, F?area?center, Return),
	send(ND, destroy),
	Return \== @nil.

print(F) :->
	send(F, lattice_noempty),
	ok_normalize(F),
	new(D, dialog('Print options')),
	send(D, transient_for, F),
	send(D, modal, transient),
	send(D, pen, 0),

	send(D, append, new(T, menu(options, marked))),
	send_list(T, append, [windows_printer, postscript]),
	send(T, layout, horizontal),
	send(D, append, button(ok, message(D, return, T?selection))),
	send(D, append, button(cancel, message(D, return, @nil))),
	send(D, default_button, ok),
	get(D, confirm_centered,
						F?area?center, Return),
	send(D, destroy),
	Return \== @nil,
	Return == windows_printer
	-> 	print_windows(F) 
	;   postscript(F).

clear_graph(F) :->
	"Clear the diagram"::
	get(F, member, picture, P),
	send(F, graph_noempty, P),
	ok_delete(F),
	send(P, clear),
	wipe_module(lat_graph).

find(F) :->
	"Find a node"::
	get(F, member, picture, P),
	send(F, graph_noempty, P),
	new(ND, dialog('Enter Node Name')),
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),

	send(ND, append, new(TI, text_item(name))),
	send(ND, append, button(ok, message(ND, return, TI?selection))),
	send(ND, append, button(cancel, message(ND, return, @nil))),
	send(ND, default_button, ok),

	get(ND, confirm_centered,
						F?area?center, Return),
	send(ND, destroy),
	Return \== @nil,
	get(Return, strip, Name),

	(	get_node_member(Name, NName)
	->  get(P, member(NName), Node),
		get(Node, position, point(NodeX, NodeY)),
		get(P, visible, area(_, _, W, H)),
		NewX = NodeX - W/2,
		NewY = NodeY - H/2,
		send(P, scroll_to, point(NewX, NewY)),
		send(F, report, status, 'Node located')
	;	send(F, report, status, 'Node not found')
	).

complete(F) :->
	lat_graph:top(T),
	get(F, node, T, TNode),
	lat_graph:bot(B),
	get(F, node, B, BNode),

	get_no_connected(L),
	connect_list_top(F, TNode, L),
	connect_list_bot(F, BNode, L),

	get_no_connected_top(ListT),
	connect_list_top(F, TNode, ListT),

	get_no_connected_bot(ListB),
	connect_list_bot(F, BNode, ListB).

connect_list_top(_, _, []).
connect_list_top(F, Top, [H|L]):-
	get(F, node, H, HNode),
	make_connect(HNode, Top, program),
	connect_list_top(F, Top, L).

connect_list_bot(_, _, []).
connect_list_bot(F, Bottom, [H|L]):-
	get(F, node, H, HNode),
	make_connect(Bottom, HNode, program),
	connect_list_bot(F, Bottom, L).

normalize(F) :->
	"Normalize the graph"::
	send(F, draw_graph),
	send(F, lattice),
	fill_from_lattice(F),
	send(F, set_edit_mode_off),
	send(F, report, status, 'Graph has been normalized').

draw_graph(F) :->
	(	prepare_for_drawing,
		send(F, layout)
		;	lat_graph:members(L),
			layout_no_connected(F, L)
		).

layout(F) :->
	get(F, member, picture, P),
	send(P, clear),
	send(P, scroll_to, point(0, 0)),
	(	lat_graph:top(_)
	->  true
	;   get_top(T),
		assert_top(T)
	),
	(   lat_graph:bot(_)
	->  true
	;   get_bot(B),
		assert_bot(B)
	),
	get_no_connected(LNC),
	layout_no_connected(F, LNC),

	lat_graph:bot(B),
	LBottom = [(B, 1)],
	layering(LBottom, L, MaxLayer, MaxNodes),
	term_to_atom(MaxNodes, AtomMN),
	atom_number(AtomMN, MN),
	send(F, max_nodes_layer, MN),

	lat_graph:top(Top),
	get_node_layer(L, Top, Layer),
	term_to_atom(Layer, AtomLy),
	atom_number(AtomLy, NLy),
	(	NLy == 0
	->  NLayer is MaxLayer + 2
	;   NLayer is NLy - 1
	),
	add_semi_connected(L, NLayer, LL),

	draw_list(F, LL),
	darcs(F).

assert_top(Node) :-
	asserta(lat_graph:top(Node)).
assert_bot(Node) :-
	asserta(lat_graph:bot(Node)).

lattice_to_graph(F) :->
	get(F, member, view, V),
	get(V, text_buffer, B),
	get(B, modified, M),

	tmp_file_stream(text, File, Stream),
	close(Stream),
	send(B, save, File),
	(	(	copy_module(lat_graph, lat_redrawn); true	),
		wipe_module(lat_graph),
		lat_graph:consult(File)
	->	(	validate_lattice(lat_graph)
		->	send(F, draw_graph)
		;	send(F, report, error, 'Lattice in view hasn\'t a correct lattice format')
		)
	;	send(F, report, error, 'Lattice in view has errors')
	),
	send(B, modified, M),
	send(F, redrawn, @on).

undo_send_graph(F) :->
	get(F, redrawn, @on),
	validate_lattice(lat_redrawn),
	copy_module(lat_redrawn, lat_graph),
	send(F, draw_graph).

validate_lattice(M) :-
	M:members(L),
	no_duplicates(L).
	
no_duplicates([]).
no_duplicates([H|T]) :-
	\+ select(H, T, _),
	no_duplicates(T).
	
lattice(F) :->
	%% "Temporaly in buffer"
	tmp_file_stream(text, File, Stream),
	close(Stream),
	new(B, text_buffer),
	send(F, compose_buffer, B),
	send(B, save, File),
	wipe_module(lat),
	(	lat:consult(File)
	;	send(F, report, error, 'Consult result: lattice in use has predicate errors')
	),
	get(F, member, view, V),
	send(V, clear),
	send(V, text_buffer, B).

compose_buffer(F, B) :->
	%% "What I have in lat_graph module"
	lat_graph:members(ListofNodes),
	with_output_to(codes(Codes), write(ListofNodes)),
	string_to_list(String, Codes),
	write_in_buffer(B, 'members(%s).\n', String),

	setof(Node, lat_graph:member(Node), L1),
	maplist(format_and_write(B), L1),

	(	lat_graph:top(Top); get_top(Top)	),
	write_in_buffer(B, 'top(%s).\n', Top),

	(	lat_graph:bot(Bottom); get_bot(Bottom)	),
	write_in_buffer(B, 'bot(%s).\n', Bottom),

	setof((From, To), lat_graph:arc(From, To), L4),
	maplist(write_arc_in_buffer(B), L4),

	%% "FROM prepare_for_drawing"
	%write_in_buffer(B, 'leq(X, X).\n'),
	%write_in_buffer(B, 'leq(X, Y):- arc(X, Z), leq(Z, Y).\n'),
    
    % Get layer list
    lat_graph:bot(Bot),
    LBottom = [(Bot, 1)],
    layering(LBottom, L, MaxLayer, _),
    % Write levels and distance in buffer
    maplist(write_level_in_buffer(B,L,MaxLayer), L1),
    %write_in_buffer(B,'distance(X,Y,Z):-level(X,L1), level(Y,L2), Z is abs(L1 - L2).\n'),
    
	%% "What I have in view"
	get(F, member, view, V),
	pce_open(V, read, File),
		repeat,
	(   read_term(File, Term, [variable_names(LVars)]),
		Term \== end_of_file
	->  with_output_to(atom(Atom), 
				write_term(Term, [variable_names(LVars), quoted(true)])),
		analize_predicate(Atom, (Name, Arity, _, _)),
		(	filtered(Name, Arity)
		->  fail
		;   write_in_buffer(B, '%s.\n', Atom)
		),
		fail
	;	!
	),
	close(File).

filtered(members, 1).
filtered(member, 1).
filtered(top, 1).
filtered(bot, 1).
filtered(arc, 2).
filtered(level,2).
%filtered(leq, 2).
%filtered(distance,3).

analize_predicate(Atom, (Name, Arity, Head, Body)):-
	atomic_list_concat([Head, Body|_], ':-', Atom), !,
	term_to_atom(TermH, Head),
 	functor(TermH, Name, Arity).
	
analize_predicate(Atom, (Name, Arity, Head, Body)):-
	term_to_atom(TermH, Atom),
 	functor(TermH, Name, Arity),
	Head = Atom,
	Body = true.

format_and_write(B, Name):-
	% To get float point '.'
	with_output_to(codes(Codes), write(Name)),
	string_codes(String, Codes),
	write_in_buffer(B, 'member(%s).\n', String).
	
write_in_buffer(Buffer, Text, Name):-
	send(Buffer, append,  string(Text, Name)).
write_in_buffer(Buffer, Text):-
	send(Buffer, append,  Text).
write_arc_in_buffer(Buffer, (N1,N2)):-
	atomic_list_concat(['arc(', N1, ', ', N2, ').\n'], Text),
	send(Buffer, append, Text).
write_level_in_buffer(Buffer,L,MaxLayer,Node) :-
    % Get node's layer number
    get_node_layer(L, Node, Layer),
	term_to_atom(Layer, AtomLy),
	atom_number(AtomLy, NLy),
	(	NLy == 0
	->  NLayer is MaxLayer + 2
	;   NLayer is NLy - 1
	),
    atomic_list_concat(['level(', Node, ', ', NLayer,').\n'], Text),
    % Assert the new predicate
    assertz(lat_graph:level(Node,NLayer)),
	send(Buffer, append, Text).


replace_in_lattice(F, Old, New):-
	get(F, member, view, V),
	get(V, editor, E),
	send(E, exact_case, @on),
	get(V, text_buffer, B),
	get(B, length, Length),
	atom_length(Old, Chars),
	Int = 0,
	repeat,
	(	get(B, find, Int, Old, 1, start, @on, @on, Index),
		Index < Length
	->	send(B, delete, Index, Chars),
		send(B, insert, Index, New),
		Int = Index + 1,
		send(F, report, 'Truth degree replaced in lattice'),
		fail
	;	!
	).

get_top(T) :-
	lat_graph:arc(_,T), \+ lat_graph:arc(T,_).
get_bot(B) :-
	lat_graph:arc(B,_), \+ lat_graph:arc(_,B).

bye(F) :->
	ok_changes(F),
	clear_memory,
	send(F, destroy).

clearspace(F) :->
	"Clear the work space"::
	clear_memory,
	get(F, member, picture, P),
	get(F, member, view, V),
	send(P, clear),
	send(V, clear),

	get(F, member(dialog_eval), D),
	get(D, member, aggregators, Aggr),
	send(Aggr, clear),
	send_list(Aggr, append, [empty]),

	get_container_combo(D, C),
	forall(between(1, 6, I),
		(	get_term_name(I, StrText),
			get(C, member(StrText), DM),
			get_var_name(I, Var),
			send(DM, clear),
			send_list(DM, append, [Var])
		)),
	get(D, member, view, DV),
	send(DV, clear),
	wipe_module(lat_redrawn),
	send(F, redrawn, @off),
	send(F, max_nodes_layer, 0),
	send(F, report, status, '').

clear_memory :-
	wipe_module(lat_graph).

newnode(F, Position) :->
	get_selected_node(F),
	get(F, nodePos, noname, Position, NewNode),
	send(NewNode, rename).

nodePos(F, Name, Position, Node:lattice_node) :<-
	"In user mode, get (create) node with specified name"::
	get(F, member, picture, P),
	(	get(P, member, Name, Node)
	->  true
	;  	send(P, display, new(Node, lattice_node(Name, green)), Position)
	).

node(F, Name, Layer, Color, NNodes, Indx, Node:lattice_node) :<-
	"In layering mode, get (create) node with specified name"::
	get(F, member, picture, P),
	get(F, max_nodes_layer, NMax),
	(	get(P, member, Name, Node)
	->  true
	;	get(P, visible, area(_, _, W, H)),
		W1 is (NMax + 1) * 80,
		(	W1 > W, W2 is W1; W2 is W	),
		MX is W2 / (NNodes + 1),
				
		with_output_to(codes(Codes), write(Name)),
		length(Codes, Length),
		% 4 characters inside the circle
		(	Length > 4, MoveX is 6 * (Length - 4) // 2; MoveX is 0	),

		NX is MX * Indx - MoveX,
		NY is H - Layer * 60,
		send(P, display, new(Node, lattice_node(Name, Color)), point(NX, NY))
	).

fill_from_lattice(F) :-
	get(F, member(dialog_eval), Oper),
	
    create_predicates(Oper,aggregators),
    create_predicates(Oper,second),
	
	lat_graph:members(L),
	maplist(atom_string, L, L1),
	get_container_combo(Oper, Container),
	fill_dragmenu(Container, 1, 6, L1),
	get_container_optgroup(Oper,Dist_combo),
    fill_dragmenu(Dist_combo, 1, 2, L1).

% Fill the Aggregator Combobox given
create_predicates(Oper,Combo_name) :- 
    get(Oper, member, Combo_name, Aggr),
	send(Aggr, clear),
    % creates a list of prolog_predicate
	findall(NewPred, operator(_, NewPred), ListPP),
	(	ListPP == []
	->  send_list(Aggr, append, [empty])
	;   send_list(Aggr, append, [noselection]),
		maplist(fill_combo_aggr(Aggr), ListPP),
		send(Aggr, sort),
		send(Aggr, selection, noselection)
	).
    
get_name_for_file(F, Extension, Path) :-	
		atomic_concat('.', Extension, Ext),
    (	get_filename(F, FileName)
	->	file_base_name(FileName, JustName),
		file_directory_name(FileName, DirName),
		file_name_extension(BaseName, _, JustName),
		file_name_extension(BaseName, Ext, OutName),
		directory_file_path(DirName, OutName, _)
	;	Path = @nil
	),
	get(@finder, file, @off, Extension, DirName, BaseName, Path),
	(   exists_file(Path)
	->  get(F, msgbox, string('%s file already exists. Rewrite it?', Ext), RT),
		RT \= @nil
	;   true
	).

msgbox(F, Msg, Ret) :<-
	new(ND, dialog('Lattice control')),
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),

	send(ND, append, new(L, label(alert, Msg))),
	send(L, alignment, center),
	send(ND, append, button(ok, message(ND, return, ok))),
	send(ND, append, button(cancel, message(ND, return, @nil))),
	send(ND, default_button, cancel),

	get(ND, confirm_centered,
						F?area?center, Ret),
	send(ND, destroy).

operator(P, P/Arity) :-
	current_predicate(lat:P/Arity),
	name(P, PN),
	(	append("and_", _, PN)
	;   append("or_", _, PN)
	;   append("agr_", _, PN)
	).

fill_combo_aggr(CB, N/A) :-
	name(N, NN),
	name(A, NA),
	change_item(NN, NewN),
	append(NewN, "/", NT),
	append(NT, NA, NewOP),
	name(TempNO, NewOP),
	send(CB, append, TempNO).

change_item(Oper, NewOper) :-
	append("and_", SN, Oper),
	append("&_", SN, NewOper).
change_item(Oper, NewOper) :-
	append("or_", SN, Oper),
	append("|_", SN, NewOper).
change_item(Oper, NewOper) :-
	append("agr_", SN, Oper),
	append("@_", SN, NewOper).

add_connective(F) :->
	send(F, get_edit_mode)
	->	new(ND, dialog('New Aggregator')),
		send(ND, transient_for, F),
		send(ND, modal, transient),
		send(ND, pen, 0),

		add_label(ND, infor, 'Select the aggregator to add to the lattice',
							normal, blue, 12),
		send(ND, append, new(CBO, menu(aggregators, cycle))),
		send_list(CBO, append, ['&_godel', '|_godel', '&_luka', '|_luka']),
		send_list(CBO, append, ['&_prod', '|_prod', '@_aver']),
		send(CBO, alignment, center),
		send(ND, append, button(add, message(ND, return, CBO?selection))),
		send(ND, append, button(cancel, message(ND, return, @nil))),
		send(ND, default_button, add),

		get(ND, confirm_centered,
							F?area?center, Return),
		send(ND, destroy),
		Return \== @nil,
		get(Return, strip, Name),
		name(Name, AA),
		change_item(Pred, AA),
		name(NA, Pred),
		get(F, member, view, V),
		append_in_view(V, NA),
		send(F, set_modified_state),
		send(F, report, status, '')
	;	send(F, report, status, 'Editor is in read-only mode').

clear_editor(F) :->
	send(F, get_edit_mode)
	->	ok_delete(F),
		get(F, member, view, V),
		send(V, clear),
		wipe_module(lat),
		send(F, report, status, '')
	;	send(F, report, status, 'Editor in read-only mode').

append_in_view(V, Aggr) :-
	Aggr == 'and_godel',
	send(V, append, 'and_godel(X,Y,Z) :- pri_min(X,Y,Z).\n').
append_in_view(V, Aggr) :-
	Aggr == and_luka,
	send(V, append, 'and_luka(X,Y,Z) :- pri_add(X,Y,U1), pri_sub(U1,1,U2), pri_max(U2,0,Z).\n').
append_in_view(V, Aggr) :-
	Aggr == 'and_prod',
	send(V, append, 'and_prod(X,Y,Z) :- pri_prod(X,Y,Z).\n').
append_in_view(V, Aggr) :-
	Aggr == or_godel,
	send(V, append, 'or_godel(X,Y,Z) :- pri_max(X,Y,Z).\n').
append_in_view(V, Aggr) :-
	Aggr == or_luka,
	send(V, append, 'or_luka(X,Y,Z) :- pri_add(X,Y,U1), pri_min(U1,1,Z).\n').
append_in_view(V, Aggr) :-
	Aggr == or_prod,
	send(V, append, 'or_prod(X,Y,Z) :- pri_prod(X,Y,U1), pri_add(X,Y,U2), pri_sub(U2,U1,Z).\n').
append_in_view(V, Aggr) :-
	Aggr == agr_aver,
	send(V, append, 'agr_aver(X,Y,Z) :- pri_add(X,Y,U1), pri_div(U1,2,Z).\n').

% Get the Aggregator selected in the combobox
get_aggregator(F,D,Combo_name,Name,NumA) :-
    get(D, member, Combo_name, Aggr),
    get(Aggr, selection, A),
    (   (A == noselection ; A == empty)
    ->  send(F, report, error, 'Please, select an aggregator.'),Name = ''
    ;   name(A, AA),
        change_item(Pred, AA),
        name(NA, Pred),
        atomic_list_concat([Name, Arity], '/', NA),
        atom_number(Arity, NumA)
    ).

eval_selected_aggregator(F) :->
	get(F, member(dialog_eval), D),
	get(D, member, view, V),

	current_output(Old),
	pce_open(V, write, Fd),
	set_output(Fd),

    get_aggregator(F,D,aggregators,Name,NumA),
		append_param(D, NumA, [], LParams),
		(	call_aggregator(Name, LParams, L)
		->  maplist(show_result(LParams), L)
		;   write(false)
		),
		send(F, report, status, '%s aggregator evaluated.', Name),
	close(Fd),
	set_output(Old),
    send(V, editable, @off).

append_param(D, I, L, NewL):-
	I < 1
	->  NewL = L
	;   get_term_name(I, StrTerm),
		get_container_combo(D, C),
		get(C, member, StrTerm, T),
		get_var_name(I, X),
		get_selection(T, S, X),
		append([S], L, L1),
		J is I - 1,
		append_param(D, J, L1, NewL).

test_selected_aggregator(F) :->
	get(F, member(dialog_eval), D),
	get_container_optgroup(D, COpt),
	get(COpt, member(evaluation), E),
	get(E, selection, Prop),
	get(D, member, view, V),
    
	send(V, clear),
	current_output(Old),
	pce_open(V, write, Fd),
    
	set_output(Fd),
    
    get_aggregator(F,D,aggregators,Name,_),
    
    (   (Prop == switchness ; Prop == distributivity)
        -> get_aggregator(F,D,second,Name2,_),call(Prop,Name,Name2)
        ; call(Prop,Name)
    ),
	close(Fd),
	set_output(Old),
    send(V, editable, @off),
	send(F, report, status, '%s aggregator tested.', E?selection).

eval_distance(F) :-> 
    get(F, member(dialog_eval), D),
	get(D, member, view, V),
    
    send(V, clear),
	current_output(Old),
	pce_open(V, write, Fd),
    set_output(Fd),
    
    get_dist_terms(D,E1,E2),
    lat_graph:distance(E1,E2,Z),
    writef('The distance between %w and %w is %w',[E1,E2,Z]),
    
    close(Fd),
	set_output(Old),
    send(V, editable, @off).
    
    
get_dist_terms(D,E1,E2) :-
	get_term_name(1, StrTerm1),
    get_term_name(2, StrTerm2),
    get_container_optgroup(D, C),
    get(C, member, StrTerm1, T1),
    get(C, member, StrTerm2, T2),
    get_var_name(1, X1),
    get_var_name(2, X2),
    get_selection(T1, E1, X1),
    get_selection(T2, E2, X2).
    
get_node_member(Name, NName):-
	lat_graph:members(L), 
	(	member(Name, L), NName = Name	
	; 	atom_number(Name, NName), member(NName, L)
	;	atom_codes(Name, Codes),
		select(44, Codes, 46, NCodes),
		number_codes(NName, NCodes),
		member(NName, L)
	).
		
wipe_module(M) :-
	forall(current_predicate(M:P/A), retract_abolish(M:P/A)).

copy_module(M, N) :-
	wipe_module(N),
	
	M:members(ListofNodes),
	N:assertz(N:members(ListofNodes)),
	forall(M:member(Node), N:assertz(N:member(Node))),
	forall(M:top(Node), N:assertz(N:top(Node))),
	forall(M:bot(Node), N:assertz(N:bot(Node))),
	forall(M:arc(From, To), N:assertz(N:arc(From, To))),
	N:assertz(N:leq(X, X)),
	N:assertz(':-'(N:leq(X, Y), (arc(X, Z), leq(Z, Y)))),
    forall(M:level(X, L), N:assertz(N:level(X, L))),
    N:assertz(':-'(N:distance(X, Y, Z), (level(X, L1), level(Y, L2), Z is abs(L1-L2)))).
    
retract_abolish(Module:Name/Arity) :-
	(   functor(Head, Name, Arity),
		predicate_property(Module:Head, dynamic)
	)
	->	retractall(Module:Head)
	;   abolish(Module:Name/Arity).

find_in(F) :->
	get(F, member, view, V),
	send(editor_find_dialog(V), open).

ok_normalize(F) :-
	get_modified_state(F)
	->  get(F, msgbox, 'Must normalize the lattice. Normalize now?', RT),
		RT \= @nil,
		send(F, normalize)
	;   true.

ok_changes(F) :-
	get_modified_state(F)
	->  get(F, msgbox, 'Current graph has been modified. Discard changes?', RT),
		RT \= @nil
	;   true.

ok_delete(F) :-
	get(F, msgbox, 'Content will be definitely lost. Delete anyway?', RT),
	RT \= @nil.

ok_node_mode(F) :-
	get_selected_node(F)
	->  true
	;   get(F, msgbox, 'Activate NODE mode?', RT),
		RT \= @nil,
		send(F, set_selected_node).

ok_arrow_mode(F) :-
	get_selected_arrow(F)
	->	true
	;	get(F, msgbox, 'Activate ARROW mode?', RT),
		RT \= @nil,
		send(F, set_selected_arrow).

lattice_noempty(_) :->
	setof(L, lat_graph:members(L), _).
graph_noempty(F, P) :->
	send(P?graphicals, empty)
	->  send(F, report, error, 'No graph loaded'), fail
	;       true.

% "Select Node or Arrow option"
set_selected_node(F) :->
	send(F, slot, drawnode, @on),
	send(F, slot, drawarrow, @off),
	retractall(lat_graph:fromNode(_)).
set_selected_arrow(F) :->
	send(F, slot, drawnode, @off),
	send(F, slot, drawarrow, @on).
set_selected_nothing(F) :->
	send(F, slot, drawnode, @off),
	send(F, slot, drawarrow, @off).
get_selected_node(F) :-
	get(F, drawnode, @on).
get_selected_arrow(F) :-
	get(F, drawarrow, @on).
set_initial_state(F) :->
	send(F, set_new_state),
	send(F, slot, modified, @off),
	send(F, slot, editable, @off).
set_new_state(F) :->
	send(F, slot, new, @on),
	send(F, slot, modified, @off),
	send(F, slot, editable, @off).
set_nonew_state(F) :->
	send(F, slot, new, @off),
	send(F, slot, modified, @off),
	send(F, slot, editable, @off).
set_modified_state(F) :->
	send(F, slot, modified, @on).
set_nomodified_state(F) :->
	send(F, slot, modified, @off).
get_modified_state(F) :-
	get(F, modified, @on); get_modified_text_lattice(F).
get_new_state(F) :-
	get(F, new, @on).
get_modified_text_lattice(F) :-
	get(F, member, view, V),
	get(V, text_buffer, B),
	get(B, modified, @on).

toggle_edit_mode(F) :->
	get(F, member, view, V),
	(	send(F, get_edit_mode)
	->	send(F, slot, editable, @off),
		send(V, editable, @off),
		send(F, report, status, 'Editor set in read-only mode')
	;	send(F, slot, editable, @on),
		send(V, editable, @on),
		send(F, report, status, 'Editor set in read-write mode')
	).
get_edit_mode(F) :->
	get(F, slot, editable, @on).
set_edit_mode_on(F) :->
	get(F, member, view, V),
	send(V, editable, @on),
	send(F, slot, editable, @on).
set_edit_mode_off(F) :->
	get(F, member, view, V),
	send(V, editable, @off),
	send(F, slot, editable, @off).
ok_edit_mode(F) :-
	send(F, get_edit_mode)
	->  true
	;	get(F, msgbox, 'Editor must be in write mode to rename truth degree references. Activate now?', RT),
		RT \= @nil,
		send(F, set_edit_mode_on).

set_filename(F, Name) :->
	send(F, slot, filename, Name).
get_filename(F, Name) :-
	get(F, filename, Name).

get_container_combo(D, G11):-
	get(D, member, group1, G1),
	get(G1, member, group11, G11).
get_container_optgroup(D, G12):-
	get(D, member, group1, G1),
	get(G1, member, group12, G12).

:- pce_end_class.
