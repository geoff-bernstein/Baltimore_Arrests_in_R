---
title: "RMD Template"
author: "sulgrave"
date: 1 Jan 2018
output:
  pdf_document:
    toc: true
  md_document:
    variant: markdown_github
  html_document:
    toc: true
    toc_depth: 10
    number_sections: true
  word_document: 
    toc: true
---

<!--output options: md_, word_ pdf_ or html_document
            beamer_, ioslides, or slidy_presentation-->

```{r}
## libraries  =====================================================================================
library(readr)
library(ggplot2)
library(dplyr)
```

```{r}
Post an r code block
'''{r, with the following arguments}

results = 'hide'      runs code in background           no display
echo = FALSE          hides only code itself            evaluates code and displays output 
eval = FALSE          displays only code itstelf        no evaluation and no output
engine = 'python'     displays as pythoncode
```


inline r code is sexy: `r factorial(4)` 

<br><br>
      
## Header 2: Some LaTeX Examples{#css_id}
$$ ozone = \alpha + \beta_{1} temperature + \sum_{locales} \beta_{i} locale_{i} + \sum_{locales} \beta_{j} interaction_{j} + \epsilon$$

$$ \min_{\beta_0,\beta} \frac{1}{N} \sum_{i=1}^{N} w_i l(y_i,\beta_0+\beta^T x_i) + \lambda\left[(1-\alpha)||\beta||_2^2/2 + \alpha ||\beta||_1\right]$$


> 
The elastic-net penalty is controlled by $\alpha$, and bridges the gap between lasso $\alpha = 1$, the default and ridge, $\alpha = 0$. 
> The tuning parameter $\lambda$ controls the overall strength of the penalty.
  
  <br><br><br>
  
### Header 3: Displaying and Modifying Text (without a slide break)
W/ Plain text, End a line with two spaces to start a new paragraph.  
*italics* and **bold**  
Emphasized text <em>italicizes and jumps out</em>  
`verbatim code in monospace text`    
sub~2~ / superscript^2^  
~~strikethrough~~  
==highlight text==   
escaped characters: \* \_ \\  
endash: --, emdash: ---  
in-line equation: $A = \pi*r^{2}$  
equation block: $$E = mc^{2}$$  
<!--Text comment not printed to output-->
\textbf{Tex ignored in HTML}  
URLs with <http://www.rstudio.com> or [link](www.rstudio.com) 

<br>

#### Header 4 Listed Items
unordered lists can be made with *, -, or +

- **bullet 1**
    - one more item
- *bullet 2* 
    - one more item
        - one more item

1. Manuallly ordered list
2. item 2
      i) sub-item  
          A. sub-sub-item 


Or Automatically ordered lists:
1. items
1. are
1. automatically
1. ordered
    - nested
        * items
        
(@) An Ordered List  
(@) whose numbering  
continues after  
(@) an interruption 
    i) and can still have  
      A. sub items  


##### Header 5 Creating Tables
| Heading       | Heading       | Heading |
| ------------- |---------------| --------|
| content       | content       | content |
| content       | content       | content |
| content       | content       | content |

<br>
###### Header 6 is grey for some reason


image:
![Caption] (smallorb.png)


<br>
 
horizontal rule/slide break:

------  
******  

 
<br>

Bibliography Citations:
Smith cited [@smith04].
Smith cited without author [-@smith04].
@smith04 cited in line.

A footnote [^1]  

Jump to [Top](#anchor)

[^1]: Here is the footnote 

```
library(keras)  
model <- keras_model_sequential() %>%  
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu',  
                input_shape = c(28,28,1)) %>%   
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>%   
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%   
  layer_flatten() %>%   
  layer_dense(units = 128, activation = 'relu') %>%   
  layer_dense(units = 10, activation = 'softmax')  
  ```