# Χρησιμοποιούμε τη βασική εικόνα του R με υποστήριξη για το Shiny
FROM rocker/shiny:4.0.5

# Εγκατάσταση των επιπλέον R πακέτων που χρειάζεσαι
RUN R -e "install.packages(c('shinythemes', 'DT', 'ggplot2', 'caret', 'e1071', 'randomForest', 'nnet', 'cluster', 'factoextra', 'umap', 'dplyr', 'readxl', 'pROC'), repos='https://cloud.r-project.org/')"

# Αντιγραφή όλων των αρχείων της εφαρμογής στον φάκελο /srv/shiny-server/
COPY . /srv/shiny-server/

# Εκθέτουμε την θύρα στην οποία θα τρέχει η εφαρμογή (συνήθως 3838 για Shiny)
EXPOSE 3838

# Δίνουμε την εντολή να ξεκινήσει το Shiny Server
CMD ["/usr/bin/shiny-server"]
