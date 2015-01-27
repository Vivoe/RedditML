package redditml.thejustinshow.com.redditml;

import android.app.AlarmManager;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.ContentValues;
import android.content.Context;
import android.content.Intent;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.os.Bundle;
import android.os.SystemClock;
import android.support.v4.app.NotificationCompat;
import android.support.v4.view.MenuItemCompat;
import android.support.v7.app.ActionBarActivity;
import android.support.v7.widget.ShareActionProvider;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.SimpleCursorAdapter;
import android.widget.TextView;

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;


public class MainActivity extends ActionBarActivity {
    //KILL THE SENSOR!
    private ShareActionProvider mShareActionProvider;

    AlarmManager mAlarm;
    PendingIntent alarmIntent;

    SQLiteDatabase db;

    ArrayList<String> rows;
    ArrayAdapter<String> adapter;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        SQLiteOpenHelper dbHelper = new DataStore(this);
        db = dbHelper.getReadableDatabase();

        mAlarm = (AlarmManager)getSystemService(Context.ALARM_SERVICE);
        Intent intent = new Intent(this, MainService.class);
        intent.setAction(Intent.ACTION_SEND);
        intent.setType("Notify");
        intent.putExtra("sender", "MainActivity");
        alarmIntent = PendingIntent.getService(this, 0, intent, PendingIntent.FLAG_UPDATE_CURRENT);

        //Every 10 min, first one 5 sec after.
        mAlarm.setRepeating(AlarmManager.ELAPSED_REALTIME, SystemClock.elapsedRealtime()+300000, 900000, alarmIntent);

        //TextView mainView = (TextView) findViewById(R.id.mainview);

        rows = new ArrayList<String>();
        adapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_1, rows);
        ListView lv = (ListView) findViewById(R.id.datastorevalues);
        lv.setAdapter(adapter);
        populateList();
    }

    private void populateList(){
        rows.clear();
        Cursor data = db.query(true, "data", null, null, null, null, null, null, null);
        int numRows = data.getCount();
        Log.i("Mainactivity", Integer.toString(numRows));
        if (numRows <= 0) {               // Zero count means empty table.
            adapter.notifyDataSetChanged();
            return;
        }

        data.moveToFirst();                       // Always one row returned
        while (!data.isLast()) {
            rows.add(getRowString(data));
            data.moveToNext();
        }
        rows.add(getRowString(data));

        adapter.notifyDataSetChanged();
    }

    private String getRowString(Cursor data){
        return data.getInt(0) + " " +
                data.getInt(1) + " " +
                data.getFloat(2) + " " +
                data.getFloat(3) + " " +
                data.getFloat(4) + " " +
                data.getInt(5);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu){

        Log.i("mainActivity", "Share");
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_main, menu);

        MenuItem item = menu.findItem(R.id.menu_item_share);

        mShareActionProvider = (ShareActionProvider) MenuItemCompat.getActionProvider(item);

        Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);

        //populateList();
        sendIntent.putExtra(Intent.EXTRA_TEXT, listToString(rows));
        //sendIntent.putExtra(Intent.EXTRA_TEXT, "Hello world");
        sendIntent.setType("text/plain");

        setShareIntent(sendIntent);
        return true;
    }

    private String listToString(ArrayList<String> list){
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < list.size(); i++){
            sb.append(list.get(i));
            sb.append("\n");
        }
        return sb.toString();
    }

    private void setShareIntent(Intent shareIntent) {
        if (mShareActionProvider != null) {
            mShareActionProvider.setShareIntent(shareIntent);
        }
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();
        //The button.
        //noinspection SimplifiableIfStatement
//        if (id == R.id.action_settings) {
//            return true;
//        }

       switch (id) {
            case R.id.notify:
                mAlarm.cancel(alarmIntent);
                Log.i("Mainservice", "LOL im not but cancelled");
                return true;
           case R.id.repopulateList:
               populateList();
               return true;
           case R.id.ClearDB:
               db.execSQL("DELETE FROM data;");
               populateList();
       }

        return super.onOptionsItemSelected(item);
    }
}
