FROM ashish1981/x86-rbase-shiny-plumber
#
#copy application
COPY /app /srv/shiny-server/
#
#
# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh
COPY run-myfile1.R /srv/shiny-server/
COPY run-myfile2.R /srv/shiny-server/
#
# Make the ShinyApp available at port 1240
EXPOSE 1240 8000 8100
#
# Copy further configuration files into the Docker image
COPY /supervisord.conf /etc/
RUN mkdir -p /var/log/supervisord/plumber1/
RUN mkdir -p /var/log/supervisord/plumber2/
RUN chmod -R 777 /var/log/supervisord/plumber1/
RUN chmod -R 777 /var/log/supervisord/plumber2/
RUN chmod -R 777 /srv/shiny-server/
#WORKDIR /srv/shiny-server/
#
#
ENTRYPOINT ["/usr/bin/supervisord", "-c", "/etc/supervisord.conf"]