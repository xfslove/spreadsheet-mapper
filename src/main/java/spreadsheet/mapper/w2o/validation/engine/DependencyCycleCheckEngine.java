package spreadsheet.mapper.w2o.validation.engine;

import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.w2o.validation.validator.DependencyValidator;

import java.util.*;

/**
 * <pre>
 * use Tarjan's strongly connected components algorithm to check cycling,
 * in directed graph, if a strongly connected component not a isolated node, means has cycle.
 * </pre>
 * Created by hanwen on 2017/1/5.
 */
public class DependencyCycleCheckEngine {

  private Map<String, Set<String>> vGraph = new HashMap<>();
  private Stack<String> vStack = new Stack<>();
  private Map<String, Integer> vIndex = new HashMap<>();
  private Map<String, Integer> vLowLink = new HashMap<>();
  private int index;

  private boolean cycling;

  public DependencyCycleCheckEngine(Map<String, List<DependencyValidator>> validatorMap) {
    this.vGraph = DependencyEngineHelper.buildVGraph(validatorMap);
    for (String s : vGraph.keySet()) {
      vIndex.put(s, 0);
      vLowLink.put(s, 0);
    }
  }

  public boolean cycling() {
    for (String v : vGraph.keySet()) {

      if (vGraph.get(v).contains(v)) {
        cycling = true;
      }

      if (cycling) {
        return cycling;
      }

      if (vIndex.get(v) == 0) {
        strongConnect(v);
      }
    }

    return cycling;
  }

  private void strongConnect(String v) {
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

    if (cycling) {
      return;
    }

    if (Objects.equals(vLowLink.get(v), vIndex.get(v))) {

      List<String> connectedComponents = new ArrayList<>();
      String connectedComponent = null;
      while (!StringUtils.equals(connectedComponent, v)) {
        connectedComponent = vStack.pop();
        connectedComponents.add(connectedComponent);
      }

      cycling = connectedComponents.size() > 1;
    }
  }

}
