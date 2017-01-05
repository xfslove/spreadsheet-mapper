package spread.sheet.w2o.validator;

import org.apache.commons.lang3.StringUtils;

import java.util.*;

/**
 * <pre>
 * using Tarjan's strongly connected components algorithm to check cycling,
 * in directed graph, if a strongly connected component not a isolated node, means has cycle.
 * </pre>
 * Created by hanwen on 2017/1/5.
 */
public class CyclingChecker {

  private Map<String, Set<String>> vGraph = new HashMap<>();
  private Stack<String> vStack = new Stack<>();
  private Map<String, Integer> vIndex = new HashMap<>();
  private Map<String, Integer> vLowLink = new HashMap<>();
  private int index;

  private boolean cycling;

  public CyclingChecker(Map<String, Set<String>> vGraph) {
    this.vGraph = vGraph;
    for (String s : vGraph.keySet()) {
      vIndex.put(s, 0);
      vLowLink.put(s, 0);
    }
  }

  public boolean cycling() {
    for (String s : vGraph.keySet()) {

      if (cycling) {
        return cycling;
      }

      if (vIndex.get(s) == 0) {
        strongConnect(s);
      }
    }

    return cycling;
  }

  private void strongConnect(String v) {

    if (cycling) {
      return;
    }

    index++;
    vIndex.put(v, index);
    vLowLink.put(v, index);
    vStack.push(v);

    for (String w : vGraph.get(v)) {
      if (vIndex.get(w) == 0) {
        strongConnect(w);
        vLowLink.put(v, Math.min(vLowLink.get(v), vLowLink.get(w)));
      } else if (vStack.contains(w)) {
        vLowLink.put(v, Math.min(vLowLink.get(v), vIndex.get(w)));
      }
    }

    if (Objects.equals(vLowLink.get(v), vIndex.get(v))) {

      List<String> connectedComponent = new ArrayList<>();
      String pop = null;
      while (!StringUtils.equals(pop, v)) {
        pop = vStack.pop();
        connectedComponent.add(pop);
      }

      cycling = connectedComponent.size() > 1;
    }
  }

}
