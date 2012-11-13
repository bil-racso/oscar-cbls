package oscar.cbls.routing.visual;

import oscar.cbls.routing.heuristic.HeuristicTimer;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.concurrent.Semaphore;

/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by Ghilain Florent.
 ******************************************************************************/


public class Dashboard extends JPanel {
    JButton nextIte = null;
    public JButton start = null;
    JScrollPane routeScroll = null;
    JTextArea route = null;
    public JComboBox heuristic = null;
    public JTextField nbNodes = null;
    public JTextField nbVehicle = null;
    public JComboBox instances = null;
    JButton makeInstance = null;
    JButton resetInstance = null;
    public JComboBox neighborhood = null;
    public JTextField klimited = null;

    JCheckBox writeRoute = null;
    public Boolean pause = true;
    public Boolean iteration = false;
    Semaphore semPause = new Semaphore(0);
    Semaphore semSyncPause = new Semaphore(0);
    public Boolean firstIte = true;
    JPanel neighborhoodPanel = null;
    JLabel neighborLabel = null;
    JPanel constraintsPanel = null;
    JRadioButton strongCButton = null;
    JRadioButton weakCButton = null;
    JTextField strongCField = null;
    JTextField weakCField = null;
    JTextField penaltySCField = null;
    JTextField penaltyWCField = null;


    JProgressBar progressBar = new JProgressBar(0, 100);;
    JFrame frameProgressBar;

    /**
     * Thread bounded to the progress bar.
     */
    private class ProgressThread implements Runnable {
       public void run(){
           frameProgressBar.setVisible(true);
           frameProgressBar.setLocationRelativeTo(PanelVRP.PanelVRP().mapPanel());

            while(HeuristicTimer.getPercentComplete() != 100){
                   try{
                       HeuristicTimer.lock();
                       progressBar.setValue(HeuristicTimer.getPercentComplete());
                       progressBar.repaint();
                   }
                   catch(Exception e){}
               }
               progressBar.setValue(0);
               HeuristicTimer.setPercentComplete(0);
               frameProgressBar.setVisible(false);
       }

    }

    public Dashboard(){

        setBackground(Color.white);
        setLayout(new FlowLayout());
        setPointsAndVehicles();
        setHeuristic();
        setInstances();

        setResetInstance();
        setStartButton();
        setNextIteButton();
        setLogRoute();
        setRouteLabel();

        setNeighborhood();
        setProgressBar();
        setConstraintsPanel();
    }

    public void setConstraintsPanel(){
        constraintsPanel = new JPanel();
        constraintsPanel.setBackground(Color.white);
        constraintsPanel.setPreferredSize(new Dimension(350,100));
        JLabel strongC = new JLabel("Strong constraints");
        strongCButton = new JRadioButton();
        strongCButton.setBackground(Color.white);
        strongCButton.setSelected(false);
        strongCField = new JTextField("0");
        strongCField.setPreferredSize(new Dimension(30,20));
        JLabel maxNodeC = new JLabel("Max nodes:");
        JLabel penaltyStrongC = new JLabel("Penalty:");
        penaltySCField = new JTextField("0");
        penaltySCField.setPreferredSize(new Dimension(30,20));

        JLabel weakC = new JLabel("Weak constraints");
        weakCButton = new JRadioButton();
        weakCButton.setSelected(false);
        weakCButton.setBackground(Color.white);
        weakCField = new JTextField("0");
        weakCField.setPreferredSize(new Dimension(30,20));
        JLabel minNodeC = new JLabel("Min nodes:");
        JLabel penaltyWeakC = new JLabel("Penalty:");
        penaltyWCField = new JTextField("0");
        penaltyWCField.setPreferredSize(new Dimension(30,20));

        constraintsPanel.add(strongC);
        constraintsPanel.add(strongCButton);
        constraintsPanel.add(maxNodeC);
        constraintsPanel.add(strongCField);
        constraintsPanel.add(penaltyStrongC);
        constraintsPanel.add(penaltySCField);
        constraintsPanel.add(weakC);
        constraintsPanel.add(weakCButton);
        constraintsPanel.add(minNodeC);
        constraintsPanel.add(weakCField);
        constraintsPanel.add(penaltyWeakC);
        constraintsPanel.add(penaltyWCField);

        add(constraintsPanel);
    }

    public void setProgressBar(){
        frameProgressBar = new JFrame();
        progressBar.setOpaque(true);
        frameProgressBar.setContentPane(progressBar);
        frameProgressBar.setUndecorated(true);
        frameProgressBar.pack();
        frameProgressBar.setVisible(false);
     }

    public void setNextIteButton(){
        nextIte = new JButton("Next iteration");
        nextIte.setBackground(Color.white);
        nextIte.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(firstIte){
                    //new Thread(new VisualMapVRP.Search()).start();
                    firstIte = false;
                    iteration = true;
                    PanelVRP.startSearching();

                }
                if(pause){
                    iteration = true;
                    unlock();
                }
            }
        });
        add(nextIte);
    }

    public void setNeighborhood(){

        neighborLabel = new JLabel();
        neighborLabel.setText("Neighbor : ");
        neighborLabel.setBackground(Color.white);
        neighborhood = new JComboBox(new String[]{"OnePointMove","ReinsertPoint","RemovePoint","Swap","ThreeOpt (no reverse)"
                ,"ThreeOpt (one reverse)","ThreeOpt (two reverse)","TwoOpt"});
        neighborhood.setBackground(Color.white);

        JLabel klimitedLabel = new JLabel();
        klimitedLabel.setText("K-limited : ");
        klimitedLabel.setBackground(Color.white);
        klimited = new JTextField();
        klimited.setText("20");
        klimited.setPreferredSize(new Dimension(35,25));
        klimited.setBackground(Color.white);

        neighborhoodPanel = new JPanel();

        neighborhoodPanel.add(neighborLabel);
        neighborhoodPanel.add(neighborhood);
        neighborhoodPanel.add(klimitedLabel);
        neighborhoodPanel.add(klimited);

        neighborhoodPanel.setBackground(Color.white);
        neighborhoodPanel.setBorder(BorderFactory.createTitledBorder("Neighborhood Options"));

        add(neighborhoodPanel);
    }

    public void setResetInstance(){
        makeInstance = new JButton("Make instance");
        makeInstance.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                new Thread(new ProgressThread()).start();
                PanelVRP.makeInstance(false);
            }
        });

        makeInstance.setBackground(Color.white);
        add(makeInstance);
        resetInstance = new JButton("Reset instance");
        resetInstance.setBackground(Color.white);
        resetInstance.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                firstIte = true;
                PanelVRP.makeInstance(true);
            }});
        add(resetInstance);
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

    public void lock2(){
        try {
            semSyncPause.acquire();
        }catch(Exception e) {
            e.printStackTrace();
        }
    }

    public void unlock2() {
        semSyncPause.release();
    }

    public void setRouteLabel(){

        route = new JTextArea("Route:\n");
        route.setSize(new Dimension(400, 100));

        routeScroll = new JScrollPane();
        routeScroll.setPreferredSize(new Dimension(300, 50));
        routeScroll.getViewport().add(route);
        add(routeScroll);
    }

    public void updateRouteLabel(String line){
        route.append(line + "\n");
        route.setCaretPosition(route.getDocument().getLength());
    }

    public void setStartButton(){
        start = new JButton("Start");
        start.setBackground(Color.white);
        start.setMaximumSize(new Dimension(100, 50));
        start.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(firstIte){
                    firstIte = false;
                    PanelVRP.startSearching();
                    lock2();
                }
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

    public void setLogRoute(){
        writeRoute = new JCheckBox();
        writeRoute.setToolTipText("Write routes (debug mode)");
        writeRoute.setBackground(Color.white);
        add(writeRoute);
    }

    public void setHeuristic(){
        heuristic = new JComboBox(new String[]{"RandomNeighbor","NearestNeighbor","Unrouted"});
        heuristic.setName("Heuristic");
        heuristic.setBackground(Color.white);
        JPanel heuristicPanel = new JPanel();
        heuristicPanel.setBackground(Color.white);
        heuristicPanel.setBorder(BorderFactory.createTitledBorder("Heuristic"));
        heuristicPanel.add(heuristic);

        add(heuristicPanel);
    }

    public void setPointsAndVehicles(){
        JLabel nodes = new JLabel("Points :");
        JLabel vehicles = new JLabel("Vehicles :");
        nbNodes = new JTextField();
        nbNodes.setPreferredSize(new Dimension(50, 25));
        nbNodes.setText("10");
        nbVehicle = new JTextField();
        nbVehicle.setPreferredSize(new Dimension(25, 25));
        nbVehicle.setText("1");

        JPanel nbInput = new JPanel();
        nbInput.setBackground(Color.white);
        nbInput.setBorder(BorderFactory.createTitledBorder("Nb points and vehicles"));
        nbInput.add(nodes);
        nbInput.add(nbNodes);
        nbInput.add(vehicles);
        nbInput.add(nbVehicle);
        add(nbInput);
    }

    public void setInstances(){
        instances = new JComboBox(new String[]{"Random (several deposits)","Random (single deposit)"});
        instances.setBackground(Color.white);
        JPanel instancesPanel = new JPanel();
        instancesPanel.add(instances);
        instancesPanel.setBackground(Color.white);
        instancesPanel.setBorder(BorderFactory.createTitledBorder("Instances"));

        add(instancesPanel);

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
