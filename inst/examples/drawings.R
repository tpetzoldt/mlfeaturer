library(DiagrammeR)

grViz("digraph dataflow {
         graph [rankdir = 'TB', bgcolor='none']
           node [shape = parallelogram, penwidth=0.5, fontname = 'Open Sans' fontsize=12]
             input output

           node [shape = cds fontsize=12]
           'transform  ' scale

           edge [color = 'navy']
           {rank = same; input; 'transform  '; scale; output; }

           input -> output
           input -> scale
           input -> 'transform  ' -> scale -> output [color = tomato]
           'transform  ' -> output [tailport = 's', headport = 'ws']

}")



grViz("digraph dataflow {
         graph [rankdir = 'LR', bgcolor='none']
           node [shape = box, penwidth=0.5, fontname = 'Open Sans']
             input output
           node [shape = 'oval', penwidth=1, fontname = 'Open Sans']
             transform scale
           edge [penwidth=1.5]
              input -> transform -> scale -> output [color='red']
           edge [penwidth=1]
              input -> output[color='gray']

              input -> scale -> output [color='navy']
              input -> transform -> output[color='forestgreen']
}")




grViz("digraph dataflow {
         graph [rankdir = 'LR', bgcolor='none']
           node [shape = box, penwidth=2, fontname = 'Open Sans']
             'input', 'output'
           node [shape = 'oval']
             'scale', 'transform'
           edge [penwidth=1.5]
              input -> transform -> scale -> output
}")

grViz("digraph dataflow {
         graph [rankdir = 'LR', bgcolor='none']
           node [shape = box, penwidth=2, fontname = 'Open Sans']
             'input', 'output'
           edge [penwidth=1.5]
              input -> output
}")

grViz("digraph dataflow {
         graph [rankdir = 'LR', bgcolor='none']
           node [shape = box, penwidth=2, fontname = 'Open Sans']
             'input', 'output'
           node [shape = 'oval']
             'scale'
           edge [penwidth=1.5]
              input -> scale -> output
}")

grViz("digraph dataflow {
         graph [rankdir = 'LR', bgcolor='none']
           node [shape = box, penwidth=2, fontname = 'Open Sans']
             'input', 'output'
           node [shape = 'oval']
             'transform'
           edge [penwidth=1.5]
              input -> transform -> output
}")
