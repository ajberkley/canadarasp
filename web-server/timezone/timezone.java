import com.databerries.timezone.TimeZoneLookup;
import java.time.ZoneId;
import java.io.*;
import java.util.Scanner;
class testApp {
    public static void main(String[] args) {
	System.out.println("Loading library");
	// instanciating the library takes ~1.5sec because it loads the dataset.
	TimeZoneLookup tz = new TimeZoneLookup();
	System.out.println("Done loading library");
	Scanner scan = new Scanner(System.in);
	while(true) {
	    try {
		double lat = scan.nextDouble();
		double lon = scan.nextDouble();
		ZoneId result = tz.getZoneId(lat, lon);
		System.out.println(result);
	    } catch(java.util.NoSuchElementException e) {
		System.out.println("Bad input, shutting down");
		break;
	    } catch(Exception e) {
		System.out.println(e);
	    }
	}
    }
}




