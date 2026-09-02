Versão v7 do gerador de planilha Jabot para Quarto.

Foco desta revisão:
- remove o full-bleed com 100vw que causava estouro horizontal no notebook;
- centraliza a ferramenta dentro do frame disponível do site;
- define largura máxima segura para desktop/notebook;
- mantém a tabela com rolagem horizontal interna, sem empurrar a página;
- usa fichas de espécime em tablet/mobile;
- corrige spans do formulário em telas menores para evitar colunas implícitas e overflow;
- reduz e estabiliza a coluna de ações;
- remove o redimensionador visual do textarea na tabela.
