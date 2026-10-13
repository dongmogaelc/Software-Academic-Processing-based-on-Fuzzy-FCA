# Software-Academic-Processing-based-on-Fuzzy-FCA
The case study uses semester-A marks from the Intensive Post-secondary
Program at the International Institute of Engineering of Water and
Environment, Ouagadougou. The source workbook contains 27 students and 18
courses spanning mathematics, informatics, physics, engineering, and general
education. Student identifiers were replaced by $x_1,\dots,x_{27}$.

Two contexts are constructed from the same mark matrix $R=(r_{ij})$. The
binary context captures course validation at the institutional threshold of
10/20:
\begin{equation}
I_b(x_i,m_j)=
\begin{cases}
1, & r_{ij}\ge 10,\\
0, & r_{ij}<10.
\end{cases}
\end{equation}
The fuzzy context preserves graded performance using the transparent linear
membership function
\begin{equation}
I_f(x_i,m_j)=\mu(r_{ij})=\min\{1,\max\{0,r_{ij}/20\}\}.
\end{equation}
This choice maps the native 0--20 scale directly into $[0,1]$ and can be
replaced by a policy-driven nonlinear function in another setting; we chose
the linear form for its transparency and direct interpretability, not
because it is presumed optimal (see Section~\ref{sec:discussion} for the
sensitivity of the results to this choice).

The analysis is performed in R. The source implementation imports the
workbook with \texttt{readxl} and constructs the binary formal context with
\texttt{fcaR}. The same academic records are then transformed into a fuzzy
context for graded concept analysis. The computational sequence is therefore
reproducible at the level of data preparation, context construction, concept
extraction, and query evaluation.
