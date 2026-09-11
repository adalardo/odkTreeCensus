#######################################################
##' Copy local files or directories to ODK Collect on Android via ADB
##'
##' @param localFiles Vector of local file or directory paths to be copied.
##' @param projectUUID Optional. The ODK project UUID. If NULL, the function will try to list and use the first project found.
##' @param media Logical. If TRUE, copies the files to the "*-media" directory of the form.
##' @param formName Optional. The name of the form (without extension) to define the media folder. If NULL and media is TRUE, the function will try to infer it from the local files.
##' @return Returns TRUE if the copy was successful for all files/directories.
##' @export
pushToODKCollect <- function(localFiles, projectUUID = NULL, media = FALSE, formName = NULL) {
    # 1. Check if ADB is installed
    adbCheck <- system("adb version", ignore.stdout = TRUE, ignore.stderr = TRUE)
    if (adbCheck != 0) {
        stop("The 'adb' command was not found on the system. Please install Android Debug Bridge (ADB) and add it to your PATH.")
    }

    # 2. Check if there are connected devices
    devices <- system("adb devices", intern = TRUE)
    # The first line is "List of devices attached", subsequent lines contain devices
    connectedDevices <- devices[devices != "" & !grepl("List of devices attached", devices)]
    if (length(connectedDevices) == 0) {
        stop("No Android device connected via USB or network was detected by ADB. Make sure USB Debugging is enabled on the phone.")
    }

    # 3. Investigate the ODK Collect path on the mobile device
    # ODK Collect usually stores its data in /sdcard/Android/data/org.odk.collect.android/files/
    # or /storage/emulated/0/Android/data/org.odk.collect.android/files/
    basePaths <- c(
        "/sdcard/Android/data/org.odk.collect.android/files",
        "/storage/emulated/0/Android/data/org.odk.collect.android/files"
    )

    targetBase <- NULL
    for (bp in basePaths) {
        checkCmd <- paste0("adb shell [ -d ", bp, " ] && echo 'OK' || echo 'NO'")
        res <- system(checkCmd, intern = TRUE)
        if (any(grepl("OK", res))) {
            targetBase <- bp
            break
        }
    }

    if (is.null(targetBase)) {
        stop("Could not locate the 'org.odk.collect.android' directory on the mobile device. Is ODK Collect installed?")
    }

    # 4. Investigate the project UUID if not provided
    if (is.null(projectUUID)) {
        projectsDir <- file.path(targetBase, "projects")
        # Garante o uso de barras normais (/) para o comando ADB no Android
        projectsDir_adb <- chartr("\\", "/", projectsDir)
        listProjectsCmd <- paste0("adb shell ls ", projectsDir_adb)
        projectsList <- system(listProjectsCmd, intern = TRUE)
        # Filter out potential error messages or empty lines
        projectsList <- projectsList[projectsList != "" & !grepl("No such file", projectsList)]
        
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

    # 5. Define the final destination path
    targetDir <- file.path(targetBase, "projects", projectUUID, "forms")
    
    if (media) {
        if (is.null(formName)) {
            # Tenta inferir o nome do formulário a partir dos arquivos locais (ex: se houver um arquivo .xml ou .xlsx)
            xmlFiles <- localFiles[grepl("\\.(xml|xlsx)$", localFiles, ignore.case = TRUE)]
            if (length(xmlFiles) > 0) {
                formName <- tools::file_path_sans_ext(basename(xmlFiles[1]))
            } else {
                stop("Para copiar para a pasta de mídia (media = TRUE), é necessário especificar o 'formName' ou incluir o arquivo do formulário (.xml ou .xlsx) em 'localFiles'.")
            }
        }
        targetDir <- file.path(targetDir, paste0(formName, "-media"))
    }
    
    # Garante o uso de barras normais (/) para o comando ADB no Android
    targetDir_adb <- chartr("\\", "/", targetDir)
    
    # Ensure the destination folder exists on the phone
    system(paste0("adb shell mkdir -p ", targetDir_adb), ignore.stdout = TRUE, ignore.stderr = TRUE)

    # 6. Prepare and copy files
    filesToCopy <- c()
    if (media) {
        for (f in localFiles) {
            if (dir.exists(f)) {
                # Lista todos os arquivos recursivamente dentro do diretório
                all_files <- list.files(f, recursive = TRUE, full.names = TRUE)
                # Filtra para garantir que não estamos incluindo diretórios vazios na lista
                all_files <- all_files[!dir.exists(all_files)]
                filesToCopy <- c(filesToCopy, all_files)
            } else if (file.exists(f)) {
                filesToCopy <- c(filesToCopy, f)
            } else {
                warning("The local file or directory does not exist and will be ignored: ", f)
            }
        }
    } else {
        filesToCopy <- localFiles
    }

    for (f in filesToCopy) {
        if (!file.exists(f) && !dir.exists(f)) {
            if (!media) {
                warning("The local file or directory does not exist and will be ignored: ", f)
            }
            next
        }
        message("Copying ", basename(f), " to the mobile device...")
        pushCmd <- paste0("adb push \"", f, "\" \"", targetDir_adb, "/\"")
        status <- system(pushCmd)
        if (status != 0) {
            stop("Failed to copy: ", f)
        }
    }

    message("All files/directories were successfully copied to: ", targetDir)
    return(TRUE)
}
