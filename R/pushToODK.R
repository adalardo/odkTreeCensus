#######################################################
##' Copy local files or directories to ODK Collect on Android via ADB
##'
##' @param localDir Optional. Path to the local directory containing the form and media. If NULL, defaults to the package's "odk" folder.
##' @param projectUUID Optional. The ODK project UUID. If NULL, the function will try to list and use the first project found.
##' @param media Logical. If TRUE, copies the "*-media" directory of the form.
##' @param formName Optional. The name of the form (without extension). Defaults to "odkTreeCensusForm".
##' @return Returns TRUE if the copy was successful.
##' @export
pushToODKCollect <- function(localDir = NULL, projectUUID = NULL, media = FALSE, formName = "treeCensusForm") {
    # 1. Use checkODKConnection to verify connection and get device info
    conn <- checkODKConnection()
    targetBase <- conn["odkFormDir"]

    # 2. Define localDir default if NULL
    if (is.null(localDir)) {
        localDir <- system.file("odk", package = "odkTreeCensus")
        if (localDir == "") {
            localDir <- "inst/odk"
        }
    }

    # 3. Search for the form XML in localDir
    localXmlPath <- file.path(localDir, paste0(formName, ".xml"))
    if (!file.exists(localXmlPath)) {
        stop(paste0(formName, ".xml", " not found in ", localDir))
    }

    # 4. Investigate the project UUID if not provided
    if (is.null(projectUUID)) {
        projectsDir <- file.path(targetBase, "projects")
        projectsDir_adb <- chartr("\\", "/", projectsDir)
        listProjectsCmd <- paste0("adb shell ls ", projectsDir_adb)
        projectsList <- system(listProjectsCmd, intern = TRUE)
        projectsList <- projectsList[projectsList != "" & !grepl("No such file", projectsList) & !grepl("ls: ", projectsList)]
        
        if (length(projectsList) == 0) {
            stop("No ODK project found in: ", projectsDir)
        } else if (length(projectsList) == 1) {
            projectUUID <- projectsList[1]
            message("Automatically detected ODK project: ", projectUUID)
        } else {
            projectUUID <- projectsList[1]
            warning("Multiple ODK projects found. Using the first one: ", projectUUID, 
                    "\nAvailable projects: ", paste(projectsList, collapse = ", "))
        }
    }

    # Define target paths on device
    targetFormsDir <- file.path(targetBase, "projects", projectUUID, "forms")
    targetXmlPath <- file.path(targetFormsDir, paste0(formName, ".xml"))
    targetXmlPath_adb <- chartr("\\", "/", targetXmlPath)
    targetFormsDir_adb <- chartr("\\", "/", targetFormsDir)

    # Ensure forms directory exists
    system(paste0("adb shell mkdir -p ", targetFormsDir_adb), ignore.stdout = TRUE, ignore.stderr = TRUE)

    # Check if XML already exists on the device
    checkFormCmd <- paste0("adb shell [ -f ", targetXmlPath_adb, " ] && echo 'OK' || echo 'NO'")
    resForm <- system(checkFormCmd, intern = TRUE)
    existsOnDevice <- any(grepl("OK", resForm))

    shouldCopy <- TRUE

    if (existsOnDevice) {
        # Get local version and date
        localVersion <- "unknown"
        tryCatch({
            localXml <- xml2::read_xml(localXmlPath)
            versionAttr <- xml2::xml_attr(xml2::xml_find_first(localXml, "//*[@version]"), "version")
            if (!is.na(versionAttr)) {
                localVersion <- versionAttr
            }
        }, error = function(e) {})
        localDate <- file.info(localXmlPath)$mtime

        # Get remote version and date
        remoteVersion <- "unknown"
        remoteDate <- "unknown"
        
        tempXml <- tempfile(fileext = ".xml")
        pullCmd <- paste0("adb pull \"", targetXmlPath_adb, "\" \"", tempXml, "\"")
        system(pullCmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
        
        if (file.exists(tempXml)) {
            tryCatch({
                remoteXml <- xml2::read_xml(tempXml)
                versionAttr <- xml2::xml_attr(xml2::xml_find_first(remoteXml, "//*[@version]"), "version")
                if (!is.na(versionAttr)) {
                    remoteVersion <- versionAttr
                }
            }, error = function(e) {})
            unlink(tempXml)
        }

        dateCmd <- paste0("adb shell stat -c %y ", targetXmlPath_adb)
        resDate <- system(dateCmd, intern = TRUE)
        if (length(resDate) > 0 && !grepl("stat:", resDate)) {
            remoteDate <- trimws(resDate[1])
        } else {
            lsCmd <- paste0("adb shell ls -l ", targetXmlPath_adb)
            resLs <- system(lsCmd, intern = TRUE)
            if (length(resLs) > 0) {
                remoteDate <- trimws(resLs[1])
            }
        }

        message("The form already exists on the device.")
        message("Local version: ", localVersion, " (Modified on: ", localDate, ")")
        message("Device version: ", remoteVersion, " (Modified on: ", remoteDate, ")")
        
        ans <- readline("Do you want to overwrite the file on the device? (y/n): ")
        if (!tolower(trimws(ans)) %in% c("s", "sim", "y", "yes")) {
            message("Operation cancelled by the user. The file was not overwritten.")
            shouldCopy <- FALSE
        }
    }

    if (shouldCopy) {
        message("Copying ", basename(localXmlPath), " to the device...")
        pushCmd <- paste0("adb push \"", localXmlPath, "\" \"", targetFormsDir_adb, "/\"")
        status <- system(pushCmd)
        if (status != 0) {
            stop("Failed to copy the form: ", localXmlPath)
        }
    }

    # 5. If media = TRUE, copy the media directory
    if (media) {
        localMediaDir <- file.path(localDir, paste0(formName, "-media"))
        if (!dir.exists(localMediaDir)) {
            warning("The local media directory does not exist: ", localMediaDir)
        } else {
            targetMediaDir <- file.path(targetFormsDir, paste0(formName, "-media"))
            targetMediaDir_adb <- chartr("\\", "/", targetMediaDir)
            
            # Ensure destination folder exists on the phone
            system(paste0("adb shell mkdir -p ", targetMediaDir_adb), ignore.stdout = TRUE, ignore.stderr = TRUE)
            
            message("Copying media folder contents to the device...")
            # List all files recursively
            filesToCopy <- list.files(localMediaDir, recursive = TRUE, full.names = TRUE)
            filesToCopy <- filesToCopy[!dir.exists(filesToCopy)]
            
            for (f in filesToCopy) {
                # Determine relative path to preserve subdirectories
                relPath <- gsub(paste0("^", normalizePath(localMediaDir, mustWork = FALSE), "/?"), "", normalizePath(f, mustWork = FALSE))
                if (relPath == normalizePath(f, mustWork = FALSE)) {
                    relPath <- substring(f, nchar(localMediaDir) + 2)
                }
                
                destFile_adb <- chartr("\\", "/", file.path(targetMediaDir, relPath))
                destDir_adb <- chartr("\\", "/", dirname(destFile_adb))
                
                # Ensure subdirectories exist
                system(paste0("adb shell mkdir -p ", destDir_adb), ignore.stdout = TRUE, ignore.stderr = TRUE)
                
                pushCmd <- paste0("adb push \"", f, "\" \"", destFile_adb, "\"")
                status <- system(pushCmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
                if (status != 0) {
                    stop("Failed to copy media file: ", f)
                }
            }
            message("Media successfully copied to: ", targetMediaDir)
        }
    }

    return(TRUE)
}

##' Copy media files to ODK Collect on Android via ADB
##'
##' @param localMediaDir Path to the local media directory.
##' @param delPrevMaps Logical. If TRUE, removes old map and grid SVG files on the device.
##' @param formName Name of the form (defaults to "odkTreeCensus").
##' @return Returns a string indicating success and the number of files copied.
##' @export
mediaToODK <- function(localMediaDir, delPrevMaps = TRUE, formName = "treeCensusForm") {
    # 1. Verify connection with the Android device
    conn <- checkODKConnection()
    targetBase <- conn["odkFormDir"]

    # Detect the active ODK project UUID
    projectsDir <- file.path(targetBase, "projects")
    projectsDir_adb <- chartr("\\", "/", projectsDir)
    listProjectsCmd <- paste0("adb shell ls ", projectsDir_adb)
    projectsList <- system(listProjectsCmd, intern = TRUE)
    projectsList <- projectsList[projectsList != "" & !grepl("No such file", projectsList) & !grepl("ls: ", projectsList)]
    
    if (length(projectsList) == 0) {
        stop("No ODK project found on the device.")
    }
    projectUUID <- projectsList[1]

    # Define media folder path on the device
    targetMediaDir <- file.path(targetBase, "projects", projectUUID, "forms", paste0(formName, "-media"))
    targetMediaDir_adb <- chartr("\\", "/", targetMediaDir)

    # 2. Check if the formName-media directory exists on the device
    checkDirCmd <- paste0("adb shell [ -d ", targetMediaDir_adb, " ] && echo 'OK' || echo 'NO'")
    resDir <- system(checkDirCmd, intern = TRUE)
    existsOnDevice <- any(grepl("OK", resDir))

    # 3. If delPrevMaps = TRUE, delete files matching "map.*svg" or "grid.*svg"
    if (delPrevMaps && existsOnDevice) {
        message("Removing old maps and grids from the device...")
        system(paste0("adb shell rm -f \"", targetMediaDir_adb, "\"/map*.svg"), ignore.stdout = TRUE, ignore.stderr = TRUE)
        system(paste0("adb shell rm -f \"", targetMediaDir_adb, "\"/grid*.svg"), ignore.stdout = TRUE, ignore.stderr = TRUE)
    }

    # 4. List files in localMediaDir and subfolders
    if (!dir.exists(localMediaDir)) {
        stop("The specified local media directory does not exist: ", localMediaDir)
    }

    localFiles <- list.files(localMediaDir, recursive = TRUE, full.names = TRUE)
    localFiles <- localFiles[!dir.exists(localFiles)] # Files only, ignore empty folders

    if (length(localFiles) == 0) {
        message("No files found in: ", localMediaDir)
        return("OK - 0 files copied")
    }

    message("Files found in the local directory to copy:")
    for (f in localFiles) {
        relPath <- gsub(paste0("^", normalizePath(localMediaDir, mustWork = FALSE), "/?"), "", normalizePath(f, mustWork = FALSE))
        if (relPath == normalizePath(f, mustWork = FALSE)) {
            relPath <- substring(f, nchar(localMediaDir) + 2)
        }
        message(" - ", relPath)
    }

    # Ask user if they want to copy
    ans <- readline("Do you want to copy all listed files to the device's media folder? (y/n): ")
    if (!tolower(trimws(ans)) %in% c("s", "sim", "y", "yes")) {
        stop("Operation cancelled by the user.")
    }

    # Ensure destination folder exists on the device
    system(paste0("adb shell mkdir -p ", targetMediaDir_adb), ignore.stdout = TRUE, ignore.stderr = TRUE)

# Copy files
    copiedCount <- 0
    for (f in localFiles) {
        fileName <- basename(f)
        destFile_adb <- chartr("\\", "/", file.path(targetMediaDir, fileName))
        
        pushCmd <- paste0("adb push \"", f, "\" \"", destFile_adb, "\"")
        status <- system(pushCmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
        if (status != 0) {
            stop("Failed to copy media file: ", f)
        }
        copiedCount <- copiedCount + 1
    }
    message("Copy completed successfully!")
    return(paste("OK -", copiedCount, "files copied"))
}

##' Verify and Configure Connection with ODK Device via ADB
##'
##' This function checks if there is an Android device connected, authorized, and configured
##' for file transfer via ADB. It manages the storage of device IDs and their respective
##' friendly nicknames, validates the ODK Collect directory structure, and extracts information
##' about the main forest census form.
##'
##' @section Android Device Configuration:
##' For this function to work properly, your Android device must have USB Debugging enabled
##' and be configured to permanently allow file transfers. Follow the steps below on your phone:
##' 
##' \strong{1. Enable Developer Options:}
##' \itemize{
##'   \item Open your phone's \strong{Settings}.
##'   \item Go to \strong{About phone} (or \strong{System > About device}).
##'   \item Locate the \strong{Build number} and tap it 7 consecutive times.
##'   \item If prompted, enter your default password. A message will confirm that you are now a developer.
##' }
##' 
##' \strong{2. Enable USB Debugging:}
##' \itemize{
##'   \item Go back to the main \strong{Settings} menu and access \strong{System > Developer options} (or search directly for "Developer options").
##'   \item Enable the \strong{Developer options} switch.
##'   \item Scroll down to the \strong{Debugging} section and enable \strong{USB debugging}.
##'   \item Connect the phone to the computer via USB cable. A pop-up will appear on the phone screen asking "Allow USB debugging?". Check "Always allow from this computer" and tap \strong{Allow}.
##' }
##' 
##' \strong{3. Configure Default USB Connection for File Transfer:}
##' \itemize{
##'   \item Still in \strong{Developer options}, scroll to the \strong{Networking} or \strong{Media} section.
##'   \item Tap \strong{Default USB configuration} (or \strong{USB preferences}).
##'   \item Select the \strong{File Transfer / Android Auto} (or \strong{MTP}) option. This ensures the device always connects in the correct mode for file exchange without requiring manual intervention on every connection.
##' }
##'
##' @return Invisibly returns a named vector containing:
##'   \item{devId}{The unique Android device identifier obtained via ADB.}
##'   \item{devName}{The unique nickname assigned by the user to the device.}
##'   \item{odkFormDir}{The absolute path of the ODK Collect files folder on the device.}
##'   \item{formVersion}{The version of the installed 'odkTreeCensusForm.xml' form, or FALSE if not found.}
##'   \item{formData}{The transfer/modification date of the form on the device, or FALSE if not found.}
##' @export
checkODKConnection <- function() {
    # 1. Check if ADB is installed
    adbCheck <- system("adb version", ignore.stdout = TRUE, ignore.stderr = TRUE)
    if (adbCheck != 0) {
        stop("The 'adb' command was not found on the system. Please install Android Debug Bridge (ADB) and add it to your PATH.")
    }

    # 2. Check if there are connected and authorized devices
    devices <- system("adb devices", intern = TRUE)
    deviceLines <- devices[devices != "" & !grepl("List of devices attached", devices)]
    authorizedDevices <- deviceLines[grepl("\\bdevice\\b", deviceLines)]

    if (length(authorizedDevices) == 0) {
        stop("No connected device configured for file transfer was found. Please check the function help documentation for configuration instructions.")
    }

    # 3. Get device ID
    devId <- strsplit(authorizedDevices[1], "\\s+")[[1]][1]

    # 4. Manage device database (stored in user's home directory)
    dbFile <- file.path(path.expand("~"), ".odkTreeCensus_devices.csv")
    if (file.exists(dbFile)) {
        db <- read.csv(dbFile, stringsAsFactors = FALSE)
    } else {
        db <- data.frame(devId = character(), devName = character(), odkFormDir = character(), stringsAsFactors = FALSE)
    }

    isNewDevice <- ! (devId %in% db$devId)
    devName <- ""

    if (isNewDevice) {
        repeat {
            inputName <- readline("Enter a unique name (no spaces) for this device: ")
            inputName <- trimws(inputName)
            if (grepl("\\s", inputName) || inputName == "") {
                message("The name cannot contain spaces or be empty. Please try again.")
            } else if (inputName %in% db$devName) {
                message("This nickname has already been used for another device. Please choose another.")
            } else {
                devName <- inputName
                break
            }
        }
        message("Device connected for the first time. Storing ID and nickname.")
    } else {
        devName <- db$devName[db$devId == devId]
        message("Device already connected previously: ", devName)
    }

    # 5. Check ODK Collect path on the device
    basePaths <- c(
        "/sdcard/Android/data/org.odk.collect.android/files",
        "/storage/emulated/0/Android/data/org.odk.collect.android/files"
    )

    odkFormDir <- NULL
    for (bp in basePaths) {
        checkCmd <- paste0("adb shell [ -d ", bp, " ] && echo 'OK' || echo 'NO'")
        res <- system(checkCmd, intern = TRUE)
        if (any(grepl("OK", res))) {
            odkFormDir <- bp
            break
        }
    }

    if (is.null(odkFormDir)) {
        stop("The ODK Collect directory was not found. Please check if ODK Collect is installed and a project is configured.")
    }

    # Save or update device info in database
    if (isNewDevice) {
        newRow <- data.frame(devId = devId, devName = devName, odkFormDir = odkFormDir, stringsAsFactors = FALSE)
        db <- rbind(db, newRow)
    } else {
        db$odkFormDir[db$devId == devId] <- odkFormDir
    }
    write.csv(db, dbFile, row.names = FALSE)

    # 6. Check active projects for "odkTreeCensusForm.xml"
    projectsDir <- file.path(odkFormDir, "projects")
    projectsDir_adb <- chartr("\\", "/", projectsDir)
    listProjectsCmd <- paste0("adb shell ls ", projectsDir_adb)
    projectsList <- system(listProjectsCmd, intern = TRUE)
    projectsList <- projectsList[projectsList != "" & !grepl("No such file", projectsList) & !grepl("ls: ", projectsList)]

    formVersion <- FALSE
    formData <- FALSE

    if (length(projectsList) > 0) {
        for (proj in projectsList) {
            formPath <- file.path(projectsDir, proj, "forms", "odkTreeCensusForm.xml")
            formPath_adb <- chartr("\\", "/", formPath)
            
            checkFormCmd <- paste0("adb shell [ -f ", formPath_adb, " ] && echo 'OK' || echo 'NO'")
            resForm <- system(checkFormCmd, intern = TRUE)
            if (any(grepl("OK", resForm))) {
                # Pull form to extract version
                tempXml <- tempfile(fileext = ".xml")
                pullCmd <- paste0("adb pull \"", formPath_adb, "\" \"", tempXml, "\"")
                system(pullCmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
                
                if (file.exists(tempXml)) {
                    tryCatch({
                        xmlContent <- xml2::read_xml(tempXml)
                        versionAttr <- xml2::xml_attr(xml2::xml_find_first(xmlContent, "//*[@version]"), "version")
                        if (!is.na(versionAttr)) {
                            formVersion <- versionAttr
                        } else {
                            formVersion <- "unknown"
                        }
                    }, error = function(e) {
                        formVersion <- "error_reading"
                    })
                    unlink(tempXml)
                }
                
                # Get modification date
                dateCmd <- paste0("adb shell stat -c %y ", formPath_adb)
                resDate <- system(dateCmd, intern = TRUE)
                if (length(resDate) > 0 && !grepl("stat:", resDate)) {
                    formData <- trimws(resDate[1])
                } else {
                    lsCmd <- paste0("adb shell ls -l ", formPath_adb)
                    resLs <- system(lsCmd, intern = TRUE)
                    if (length(resLs) > 0) {
                        formData <- trimws(resLs[1])
                    } else {
                        formData <- "unknown"
                    }
                }
                break
            }
        }
    }

    result <- c(
        devId = devId,
        devName = devName,
        odkFormDir = odkFormDir,
        formVersion = as.character(formVersion),
        formData = as.character(formData)
    )

    return(invisible(result))
}
