/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cbls.routing.visual;


import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.concurrent.Semaphore;


/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by Ghilain Florent.
 ******************************************************************************/

public class Dashboard extends JPanel {

    private JButton nextIte = null;
    public JButton start = null;
    private JButton smartStart = null;
    private JScrollPane routeScroll = null;
    private JTextArea route = null;
    public JComboBox heuristic = null;
    public JTextField nbNodes = null;
    public JTextField nbVehicle = null;
    public JComboBox instances = null;
    private JButton makeInstance = null;
    private JButton resetInstance = null;
    public JComboBox neighborhood = null;
    public JTextField klimited = null;

    private JCheckBox writeRoute = null;
    public Boolean pause = true;
    public Boolean iteration = false;
    private final Semaphore semPause = new Semaphore(0);
    private final Semaphore semSyncPause = new Semaphore(0);
    public Boolean firstIte = true;
    private JPanel neighborhoodPanel = null;
    private JLabel neighborLabel = null;
    private JPanel constraintsPanel = null;
    JRadioButton strongCButton = null;
    JRadioButton weakCButton = null;
    JTextField strongCField = null;
    JTextField weakCField = null;
    JTextField penaltySCField = null;
    JTextField penaltyWCField = null;

    private PanelVRP myPanelVRP = null;

    private final JProgressBar progressBar = new JProgressBar(0, 100);
    private JFrame frameProgressBar;


    /**
     * Thread bounded to the progress bar.
     */
    private class ProgressThread implements Runnable {
        public void run(){
            frameProgressBar.setVisible(true);
            frameProgressBar.setLocationRelativeTo(myPanelVRP.mapPanel());
            /*
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
            */
            frameProgressBar.setVisible(false);
        }

    }

    public Dashboard(boolean easyMode, PanelVRP myPanelVRP){

        this.myPanelVRP = myPanelVRP;

        if(easyMode){
            setBackground(Color.white);
            setLayout(new FlowLayout());
            //setPointsAndVehicles();
            //setHeuristic();
            setInstances(easyMode);
            setMakeInstance();
            //setResetInstance();
            setStartButton();
            setNextIteButton();
            setSmartButton();
            //setLogRoute();
            //setRouteLabel();
            setNeighborhood(easyMode);
            setProgressBar();
            //setConstraintsPanel(easyMode);
        }
        else{
            setBackground(Color.white);
            setLayout(new FlowLayout());
            setPointsAndVehicles();
            setHeuristic();
            setInstances(easyMode);
            setMakeInstance();
            setResetInstance();
            setStartButton();
            setNextIteButton();
            setSmartButton();
            setLogRoute();
            setRouteLabel();

            setNeighborhood(easyMode);
            setProgressBar();
            setConstraintsPanel();
        }
    }

    void setConstraintsPanel(){
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

        JLabel blanc = new JLabel("");
        blanc.setPreferredSize(new Dimension(50,20));
        constraintsPanel.add(blanc);
        //constraintsPanel.add(penaltyStrongC);
        //constraintsPanel.add(penaltySCField);
        constraintsPanel.add(weakC);
        constraintsPanel.add(weakCButton);
        constraintsPanel.add(minNodeC);
        constraintsPanel.add(weakCField);
        constraintsPanel.add(penaltyWeakC);
        constraintsPanel.add(penaltyWCField);

        add(constraintsPanel);
    }

    void setProgressBar(){
        frameProgressBar = new JFrame();
        progressBar.setOpaque(true);
        frameProgressBar.setContentPane(progressBar);
        frameProgressBar.setUndecorated(true);
        frameProgressBar.pack();
        frameProgressBar.setVisible(false);
    }

    void setNextIteButton(){
        nextIte = new JButton("Next iteration");
        nextIte.setBackground(Color.white);
        nextIte.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(firstIte){
                    //new Thread(new OldVisualMap.Search()).start();
                    firstIte = false;
                    iteration = true;
                    myPanelVRP.startSearching();

                }
                if(pause){
                    iteration = true;
                    unlock();
                }
            }
        });
        add(nextIte);
    }

    void setNeighborhood(boolean easyMode){

        neighborLabel = new JLabel();
        neighborLabel.setText("Neighbor : ");
        neighborLabel.setBackground(Color.white);
        neighborhood = new JComboBox(new String[]{"OnePointMove","ReinsertPoint","RemovePoint","Swap","ThreeOpt (no reverse)"
                ,"ThreeOpt (one reverse)","ThreeOpt (two reverse)","TwoOpt","Composite"});
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
        if(!easyMode){
            neighborhoodPanel.add(klimitedLabel);
            neighborhoodPanel.add(klimited);
        }

        neighborhoodPanel.setBackground(Color.white);
        neighborhoodPanel.setBorder(BorderFactory.createTitledBorder("Neighborhood Options"));

        add(neighborhoodPanel);
    }

    void setMakeInstance(){
        System.out.println("created make instance");
        makeInstance = new JButton("Make instance");
        makeInstance.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                new Thread(new ProgressThread()).start();
                System.out.println("pressed make instance");
                myPanelVRP.makeInstance(false);
            }
        });

        makeInstance.setBackground(Color.green);
        add(makeInstance);
    }

    void setResetInstance(){

        resetInstance = new JButton("Reset instance");
        resetInstance.setBackground(Color.white);
        resetInstance.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                firstIte = true;
                myPanelVRP.makeInstance(true);
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

    void unlock() {
        semPause.release();
    }

    void lock2(){
        try {
            semSyncPause.acquire();
        }catch(Exception e) {
            e.printStackTrace();
        }
    }

    public void unlock2() {
        semSyncPause.release();
    }

    void setRouteLabel(){

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
    void setSmartButton(){
        smartStart =  new JButton("Smart Search");
        smartStart.setBackground(Color.green);
        smartStart.setMaximumSize(new Dimension(100, 50));
        smartStart.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (firstIte) {
                    firstIte = false;
                    myPanelVRP.startSmartSearching();
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
        add(smartStart);
    }

    void setStartButton(){
        start = new JButton("Start");
        start.setBackground(Color.green);
        start.setMaximumSize(new Dimension(100, 50));
        start.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if (firstIte) {
                    firstIte = false;
                    myPanelVRP.startSearching();
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

    void setLogRoute(){
        writeRoute = new JCheckBox();
        writeRoute.setToolTipText("Write routes (debug mode)");
        writeRoute.setBackground(Color.white);
        add(writeRoute);
    }

    void setHeuristic(){
        heuristic = new JComboBox(new String[]{"RandomNeighbor","NearestNeighbor","Unrouted"});
        heuristic.setName("Heuristic");
        heuristic.setBackground(Color.white);
        JPanel heuristicPanel = new JPanel();
        heuristicPanel.setBackground(Color.white);
        heuristicPanel.setBorder(BorderFactory.createTitledBorder("Heuristic"));
        heuristicPanel.add(heuristic);

        add(heuristicPanel);
    }

    void setPointsAndVehicles(){
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

    void setInstances(Boolean easyMode){

        if(!easyMode)
            instances = new JComboBox(new String[]{"Random (several deposits)","Random (single deposit)"});
        else{

            strongCButton = new JRadioButton();
            strongCButton.setSelected(false);
            strongCField = new JTextField("0");
            penaltySCField = new JTextField("1");


            weakCButton = new JRadioButton();
            weakCButton.setSelected(true);
            weakCField = new JTextField("10");
            penaltyWCField = new JTextField("100");


            nbNodes = new JTextField("50");
            nbVehicle = new JTextField("2");
            heuristic = new JComboBox(new String[]{"RandomNeighbor","NearestNeighbor","Unrouted"});
            heuristic.setSelectedIndex(0);
            writeRoute = new JCheckBox();

            instances = new JComboBox(new String[]{"2D-2V-50P","4D-4V-100P","6D-6V-150P","1D-8V-200P","1D-10V-250P"});
            instances.setPreferredSize(new Dimension(100,30));
            instances.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    int i = instances.getSelectedIndex();

                    nbVehicle.setText("" + (2*(i+1)));
                    nbNodes.setText(""+ ((i+1)*50));
                }
            });
            instances.setSelectedIndex(0);
        }

        instances.setBackground(Color.white);
        JPanel instancesPanel = new JPanel();
        instancesPanel.add(instances);
        instancesPanel.setBackground(Color.white);
        instancesPanel.setBorder(BorderFactory.createTitledBorder("Instances"));

        instances.setBackground(Color.white);


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
