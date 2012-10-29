package oscar.visual.vrp;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.concurrent.Semaphore;

/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 24/10/12
 * Time: 10:32
 * To change this template use InstanceVRP | Settings | InstanceVRP Templates.
 */

public class Dashboard extends JPanel {
    JButton nextIte = null;
    JButton start = null;
    JScrollPane routeScroll = null;
    JTextArea route = null;

    JCheckBox writeRoute = null;
    Boolean pause = true;
    Boolean iteration = false;
    Semaphore semPause = new Semaphore(0);

    public Dashboard(){
        setBackground(Color.white);
        setLayout(new FlowLayout());
        setStartButton();
        setNextIteButton();
        setCheckWriteBox();
        setRouteLabel();
     }

    public void setNextIteButton(){
        nextIte = new JButton("Next iteration");
        nextIte.setMaximumSize(new Dimension(100, 50));
        nextIte.setBackground(Color.white);
        nextIte.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(pause){
                    iteration = true;
                    unlock();
                }
            }
        });
        add(nextIte);
    }

    public void lock(){
        try {
            semPause.acquire();
        }catch(Exception e) {
            e.printStackTrace();
        }
    }

    public void unlock() {
        semPause.release();
    }

    public void setRouteLabel(){

        route = new JTextArea("Route:\n");
        route.setSize(new Dimension(500, 100));

        routeScroll = new JScrollPane();
        routeScroll.setPreferredSize(new Dimension(500, 100));
        routeScroll.getViewport().add(route);
        add(routeScroll);
    }

    public void updateRouteLabel(String line){
       route.append(line + "\n");
     }

    public void setStartButton(){
        start = new JButton("Start");
        start.setBackground(Color.white);
        start.setMaximumSize(new Dimension(100, 50));
        start.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                pause = !pause;
                if (pause)
                    start.setText("Restart");
                else {
                    iteration = false;
                    start.setText("Pause");
                    unlock();
                }
            }
        });
        add(start);
    }

    public void setCheckWriteBox(){
        writeRoute = new JCheckBox();
        writeRoute.setToolTipText("Write routes (debug mode)");
        add(writeRoute);
    }

    public Boolean inPause(){
        return pause;
    }

    public Boolean inIteration(){
        return iteration;
    }

    public Boolean writeRoute(){
        return writeRoute.isSelected();
    }



}
