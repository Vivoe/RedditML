package redditml.thejustinshow.com.redditml;

import android.app.IntentService;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.ContentValues;
import android.content.Context;
import android.content.Intent;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.os.Build;
import android.os.PowerManager;
import android.support.v4.app.NotificationCompat;
import android.util.Log;

import java.util.Arrays;
import java.util.Calendar;

/**
 * Created by epkfaile on 2015-01-10.
 */
public class MainService extends IntentService implements SensorEventListener {
//public class MainService extends IntentService {

    private SensorManager mSensorManager;
    private Sensor mAccel;
    float[] values;

    public MainService(){
        super("MainService");
//        Log.i("MainService", "Started!");

    }

    @Override
    public void onCreate(){
        super.onCreate();
        values = new float[3];
        mSensorManager = (SensorManager) getSystemService(SENSOR_SERVICE);
        mAccel = mSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER);
        mSensorManager.registerListener(this, mAccel, SensorManager.SENSOR_DELAY_NORMAL);
//        Log.i("MainService", "onCreate");
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        super.onStartCommand(intent, flags, startId);
        //TODO do something useful
        Log.i("MainService", "start command?");

        return Service.START_NOT_STICKY;
    }

    @Override
    protected void onHandleIntent(Intent intent){

        PowerManager pm = (PowerManager)getSystemService(POWER_SERVICE);
        if (Build.VERSION.SDK_INT < 20){
            if (!pm.isScreenOn()){
                ContentValues c = getData();
                SQLiteOpenHelper sqlHelper = new DataStore(this);
                SQLiteDatabase db = sqlHelper.getWritableDatabase();
                c.put("ans", false);
                Log.i("mainservice", c.toString());
                db.insert("data", null, c);
                return;
            }
        } else {
            if (!pm.isInteractive()){
                ContentValues c = getData();
                SQLiteOpenHelper sqlHelper = new DataStore(this);
                SQLiteDatabase db = sqlHelper.getWritableDatabase();
                c.put("ans", false);
                Log.i("mainservice", c.toString());
                db.insert("data", null, c);
                return;
            }
        }


        Log.i("MainService", "handle intents");
        if (intent == null){
            Log.i("Mainservice", "OK SRSY WTF");
        }

        String t = intent.getAction();
        if (t != null) {
            Log.i("MainService", t);
        } else {
            Log.i("MainService", "null");
            String s = intent.getStringExtra("sender");
            if (s != null) {
                Log.i("MainService", s);
            } else {
                Log.i("Mainservice", "EVERYTHING DEID");
            }
        }

        if (Intent.ACTION_ANSWER.equals(intent.getAction()) && intent.getType() != null){
            Log.i("MainService", "Adding row");
            SQLiteOpenHelper sqlHelper = new DataStore(this);
            SQLiteDatabase db = sqlHelper.getWritableDatabase();
            ContentValues row = (ContentValues) intent.getParcelableExtra("row");
            row.put("ans", intent.getBooleanExtra("response", false));
            Log.i("mainservice", row.toString());
            db.insert("data", null, row);
            Log.i("mainservice", "Inserted");
            NotificationManager n = (NotificationManager)getSystemService(NOTIFICATION_SERVICE);
            n.cancel(0);


        } else  if (Intent.ACTION_SEND.equals(intent.getAction()) && intent.getType() != null){
            //Notification stuff, get all requrired data
            //Get accel

//            if (values[0] == 0.0 && values[1] == 0.0 && values[2] == 0.0){
//                Log.i("MainService", "Retry");
//                startService(intent);
//                return;
//            }

            while (values[0] == 0.0 && values[1] == 0.0 && values[2] == 0.0){
                try {
                    Log.i("MainService", "Retry");
                    Thread.sleep(100);
                }catch(InterruptedException e){
                    e.printStackTrace();
                }
            }

            Log.i("MainService", "done thinging");

            Intent lameIntent = new Intent(this, MainActivity.class);
            lameIntent.setType(Intent.ACTION_DEFAULT);
            PendingIntent pLameIntent = PendingIntent.getActivity(this, 0, lameIntent, 0);

            Calendar c = Calendar.getInstance();
            int day = c.get(Calendar.DAY_OF_WEEK);
            int time = c.get(Calendar.HOUR_OF_DAY) * 60 + c.get(Calendar.MINUTE);
            Log.i("mainservice", day +" " +time);

            ContentValues row = new ContentValues();
            row.put("day", day);
            row.put("times", time);
            Log.i("mainservice", Arrays.toString(values));
            row.put("x", values[0]);
            row.put("y", values[1]);
            row.put("z", values[2]);

            Intent yesRecieve = new Intent(this, MainService.class);
            yesRecieve.setAction(Intent.ACTION_ANSWER);
            yesRecieve.setType("dataPoint");
            yesRecieve.putExtra("row", row);
            yesRecieve.putExtra("sender", "Mainservice");
            yesRecieve.putExtra("response", true);
            PendingIntent pIntentYes = PendingIntent.getService(this, 0, yesRecieve, PendingIntent.FLAG_ONE_SHOT);

            Intent noRecieve = new Intent(this, MainService.class);
            noRecieve.setAction(Intent.ACTION_ANSWER);
            noRecieve.setType("dataPoint");
            noRecieve.putExtra("row", row);
            noRecieve.putExtra("sender", "Mainservice");
            noRecieve.putExtra("response", false);
            PendingIntent pIntentNo = PendingIntent.getService(this, 1, noRecieve, PendingIntent.FLAG_ONE_SHOT);

            Notification n = new NotificationCompat.Builder(this)
                    .setContentTitle("RedditML")
                    .setContentText("Are you Redditing?")
                    .setSmallIcon(R.drawable.abc_btn_switch_to_on_mtrl_00012)
                    .setContentIntent(pLameIntent)
                    .addAction(R.drawable.abc_btn_switch_to_on_mtrl_00012, "Yes ):", pIntentYes)
                    .addAction(R.drawable.abc_btn_switch_to_on_mtrl_00012, "No! :D", pIntentNo).build();

            NotificationManager nManager = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);

            n.flags |= Notification.FLAG_AUTO_CANCEL;

            nManager.notify(0, n);

        }
    }

    private ContentValues getData() {
        while (values[0] == 0.0 && values[1] == 0.0 && values[2] == 0.0) {
            try {
                Log.i("MainService", "Retry");
                Thread.sleep(100);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        Calendar c = Calendar.getInstance();
        int day = c.get(Calendar.DAY_OF_WEEK);
        int time = c.get(Calendar.HOUR_OF_DAY) * 60 + c.get(Calendar.MINUTE);
        Log.i("mainservice", day + " " + time);

        ContentValues row = new ContentValues();
        row.put("day", day);
        row.put("times", time);
        Log.i("mainservice", Arrays.toString(values));
        row.put("x", values[0]);
        row.put("y", values[1]);
        row.put("z", values[2]);
        return row;
    }

    boolean first = true;
    @Override
    public final void onSensorChanged(SensorEvent event){
        //Log.i("Mainservice", Arrays.toString(event.values));
        //values = event.values.clone();
            values[0] = event.values[0];
            values[1] = event.values[1];
            values[2] = event.values[2];
       // Log.i("Mainservice sensor", Arrays.toString(values));
        if (first){
            Log.i("Mainservice sensor", "First!");
            first = false;
        }
    }

    @Override
    public final void onAccuracyChanged(Sensor sensor, int accuracy){

    }

    @Override
    public void onDestroy(){
        super.onDestroy();
        //mSensorManager.unregisterListener(this, mAccel);
    }

}
