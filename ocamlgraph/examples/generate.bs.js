// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Rand from "../src/rand.bs.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Graphviz from "../src/graphviz.bs.js";
import * as Imperative from "../src/imperative.bs.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";

var compare = Caml_obj.caml_compare;

var equal = Caml_obj.caml_equal;

var Int = {
  compare: compare,
  equal: equal,
  hash: Hashtbl.hash
};

var G = Imperative.Digraph.Abstract({});

var R = Rand.I(G);

var E = G.E;

var iter_vertex = G.iter_vertex;

var iter_edges_e = G.iter_edges_e;

function vertex_name(v) {
  return String(Curry._1(G.V.label, v));
}

function graph_attributes(param) {
  return /* [] */0;
}

function default_vertex_attributes(param) {
  return /* [] */0;
}

function vertex_attributes(param) {
  return /* [] */0;
}

function default_edge_attributes(param) {
  return /* [] */0;
}

function edge_attributes(param) {
  return /* [] */0;
}

function get_subgraph(param) {
  
}

var Display_V = G.V;

var Display_is_directed = G.is_directed;

var Display_is_empty = G.is_empty;

var Display_nb_vertex = G.nb_vertex;

var Display_nb_edges = G.nb_edges;

var Display_out_degree = G.out_degree;

var Display_in_degree = G.in_degree;

var Display_mem_vertex = G.mem_vertex;

var Display_mem_edge = G.mem_edge;

var Display_mem_edge_e = G.mem_edge_e;

var Display_find_edge = G.find_edge;

var Display_find_all_edges = G.find_all_edges;

var Display_succ = G.succ;

var Display_pred = G.pred;

var Display_succ_e = G.succ_e;

var Display_pred_e = G.pred_e;

var Display_fold_vertex = G.fold_vertex;

var Display_iter_edges = G.iter_edges;

var Display_fold_edges = G.fold_edges;

var Display_fold_edges_e = G.fold_edges_e;

var Display_map_vertex = G.map_vertex;

var Display_iter_succ = G.iter_succ;

var Display_iter_pred = G.iter_pred;

var Display_fold_succ = G.fold_succ;

var Display_fold_pred = G.fold_pred;

var Display_iter_succ_e = G.iter_succ_e;

var Display_fold_succ_e = G.fold_succ_e;

var Display_iter_pred_e = G.iter_pred_e;

var Display_fold_pred_e = G.fold_pred_e;

var Display_create = G.create;

var Display_clear = G.clear;

var Display_copy = G.copy;

var Display_add_vertex = G.add_vertex;

var Display_remove_vertex = G.remove_vertex;

var Display_add_edge = G.add_edge;

var Display_add_edge_e = G.add_edge_e;

var Display_remove_edge = G.remove_edge;

var Display_remove_edge_e = G.remove_edge_e;

var Display_Mark = G.Mark;

var Display = {
  V: Display_V,
  E: E,
  is_directed: Display_is_directed,
  is_empty: Display_is_empty,
  nb_vertex: Display_nb_vertex,
  nb_edges: Display_nb_edges,
  out_degree: Display_out_degree,
  in_degree: Display_in_degree,
  mem_vertex: Display_mem_vertex,
  mem_edge: Display_mem_edge,
  mem_edge_e: Display_mem_edge_e,
  find_edge: Display_find_edge,
  find_all_edges: Display_find_all_edges,
  succ: Display_succ,
  pred: Display_pred,
  succ_e: Display_succ_e,
  pred_e: Display_pred_e,
  iter_vertex: iter_vertex,
  fold_vertex: Display_fold_vertex,
  iter_edges: Display_iter_edges,
  fold_edges: Display_fold_edges,
  iter_edges_e: iter_edges_e,
  fold_edges_e: Display_fold_edges_e,
  map_vertex: Display_map_vertex,
  iter_succ: Display_iter_succ,
  iter_pred: Display_iter_pred,
  fold_succ: Display_fold_succ,
  fold_pred: Display_fold_pred,
  iter_succ_e: Display_iter_succ_e,
  fold_succ_e: Display_fold_succ_e,
  iter_pred_e: Display_iter_pred_e,
  fold_pred_e: Display_fold_pred_e,
  create: Display_create,
  clear: Display_clear,
  copy: Display_copy,
  add_vertex: Display_add_vertex,
  remove_vertex: Display_remove_vertex,
  add_edge: Display_add_edge,
  add_edge_e: Display_add_edge_e,
  remove_edge: Display_remove_edge,
  remove_edge_e: Display_remove_edge_e,
  Mark: Display_Mark,
  vertex_name: vertex_name,
  graph_attributes: graph_attributes,
  default_vertex_attributes: default_vertex_attributes,
  vertex_attributes: vertex_attributes,
  default_edge_attributes: default_edge_attributes,
  edge_attributes: edge_attributes,
  get_subgraph: get_subgraph
};

var Gv = Graphviz.Dot({
      V: {},
      E: {
        src: E.src,
        dst: E.dst
      },
      iter_vertex: iter_vertex,
      iter_edges_e: iter_edges_e,
      graph_attributes: graph_attributes,
      default_vertex_attributes: default_vertex_attributes,
      vertex_name: vertex_name,
      vertex_attributes: vertex_attributes,
      get_subgraph: get_subgraph,
      default_edge_attributes: default_edge_attributes,
      edge_attributes: edge_attributes
    });

Curry._2(Gv.output_graph, Pervasives.stdout, Curry._4(R.graph, undefined, 10, 13, undefined));

var v = 10;

var e = 13;

export {
  v ,
  e ,
  Int ,
  G ,
  R ,
  Display ,
  Gv ,
  
}
/* G Not a pure module */
