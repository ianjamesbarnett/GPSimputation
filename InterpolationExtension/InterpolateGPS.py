
##### User specified parameters
fildir = "C:/Users/ianja/Dropbox/MayneGPS/data" # directory where data sits
indata = "mayne_aware_export_04202022.csv" #name of the csv file containing gps data
outdir = "C:/Users/ianja/Dropbox/MayneGPS/data/Beiweformat" #directory to be created that will contain all output

intvl = 1 # number of seconds to interpolate observations
max_interp_gap = 5*60 # largest gap that we interpolate (anything longer is left as missing), in seconds
##### 

##### Imports
import os
from datetime import datetime
import pytz
##### 


    



# get list of all IDs
IID_set= set()
filein=open(fildir+"/"+indata,'r')
line=filein.readline()
for line in filein:
    line_v=line.strip().split(",")
    IID=line_v[2].strip("\"")
    if IID not in IID_set:
        IID_set.add(IID)


# construct file structure for the IDs
if not os.path.isdir(outdir):
    os.mkdir(outdir)
for IID in list(IID_set):
    if not os.path.isdir(outdir+"/"+IID):
        os.mkdir(outdir+"/"+IID)


# loop over all IDs and interpolate, writing new data
for IID in list(IID_set):
    print(IID)
    outfilename=outdir+"/"+IID+"/gps_interpolated_"+str(intvl)+"sec_"+IID[:6]+".csv"
    outdat = []
    outdat.append("timestamp,UTC time,latitude,longitude,altitude,accuracy\n")
    filein.seek(0)
    for line in filein:
        line_v=line.strip().split(",")
        if line_v[2].strip("\"") != IID:
            continue
        #timestamp	UTC time	 latitude	 longitude	 altitude	 accuracy
        timestamp = line_v[1]
        lat = line_v[3]
        lon = line_v[4]
        alt = line_v[7]
        acc = line_v[9]
        dt_object = datetime.fromtimestamp(int(timestamp)/1000)
        dt_object=pytz.utc.localize(dt_object)
        UTCtime = dt_object.strftime("%Y-%m-%dT%H:%M:%S")
        outline=",".join([timestamp,UTCtime,lat,lon,alt,acc])+"\n"
        if len(outdat)>1:
            prevts=outdat[-1].split(",")[0]
            if int(timestamp)-int(prevts)<max_interp_gap*1000 and int(timestamp)-int(prevts)>intvl*1000:
                outlinep_v = outdat[-1].split(",")
                latp=outlinep_v[2]
                lonp=outlinep_v[3]
                altp=outlinep_v[4]
                accp=outlinep_v[5]
                numinterp = int((int(timestamp)-int(prevts))/(intvl*1000))
                for i in range(numinterp):
                    timestampi = str(int(prevts)+1000*intvl*(i+1))
                    wn = float(int(timestampi)-int(prevts))/(int(timestamp)-int(prevts))
                    wp = 1-wn
                    lati = str(wn*float(lat)+wp*float(latp))
                    loni = str(wn*float(lon)+wp*float(lonp))
                    alti = str(round(wn*float(alt)+wp*float(altp),1))
                    acci = str(round(wn*float(acc)+wp*float(accp),1))
                    dt_objecti = datetime.fromtimestamp(int(timestampi)/1000)
                    dt_objecti=pytz.utc.localize(dt_objecti)
                    UTCtimei = dt_objecti.strftime("%Y-%m-%dT%H:%M:%S")
                    outlinei =",".join([timestampi,UTCtimei,lati,loni,alti,acci])+"\n"
                    outdat.append(outlinei)
        outdat.append(outline)
    # write outdat to file
    fileout=open(outfilename,'w')
    for ele in outdat:
        fileout.write(ele)
    fileout.close()




