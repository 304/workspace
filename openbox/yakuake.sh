#!/bin/bash

yakuake

qdbus org.kde.yakuake /yakuake/sessions runCommand "cd $HOME"
qdbus org.kde.yakuake /yakuake/tabs setTabTitle 1 "main"
qdbus org.kde.yakuake /yakuake/sessions addSession

qdbus org.kde.yakuake /yakuake/sessions addSession
qdbus org.kde.yakuake /yakuake/sessions runCommand "htop"
qdbus org.kde.yakuake /yakuake/tabs setTabTitle 2 htop

qdbus org.kde.yakuake /yakuake/sessions addSession
qdbus org.kde.yakuake /yakuake/sessions runCommand "py"
qdbus org.kde.yakuake /yakuake/tabs setTabTitle 3 calc

exit 0
