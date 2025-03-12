/* 1. Données d'exemple */
data heatmap;
  input X $ Y $ Value;
  datalines;
A A 0.02
A B 0.08
A C 0.03
B A 0.06
B B 0.01
B C 0.07
C A 0.04
C B 0.10
C C 0.03
;
run;

/* 2. Format personnalisé pour les couleurs */
proc format;
  value colfmt low -< 0.05 = 'green'
               0.05 - high = 'red';
run;

/* 3. Template avec variables dynamiques */
proc template;
  define statgraph HeatmapDynamique;
    dynamic _X _Y _Z;
    begingraph;
      layout overlay /
          xaxisopts=(discreteopts=(reverse=true))
          yaxisopts=(discreteopts=(reverse=true));
        heatmapparm x=_X y=_Y colorresponse=_Z /
          colormodel=(green red)
          colorstat=discrete
          discreterangeformat=colfmt.
          name='heatmap';
        continuouslegend 'heatmap';
      endlayout;
    endgraph;
  end;
run;

/* 4. Génération dynamique de la heatmap avec PROC SGRENDER */
proc sgrender data=heatmap template=HeatmapDynamique;
  dynamic _X="X" _Y="Y" _Z="Value";
run;