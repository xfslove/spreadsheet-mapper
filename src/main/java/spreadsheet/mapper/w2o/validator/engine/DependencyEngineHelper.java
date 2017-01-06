package spreadsheet.mapper.w2o.validator.engine;

import org.apache.commons.collections.CollectionUtils;
import spreadsheet.mapper.w2o.validator.DependencyValidator;

import java.util.*;

/**
 * Created by hanwen on 2017/1/6.
 */
public class DependencyEngineHelper {

  private DependencyEngineHelper() {
    // default constructor
  }

  /**
   * create directed graph of dependency validators
   *
   * @param validatorMap dependency validators
   * @return the directed graph
   */
  public static Map<String, Set<String>> buildVGraph(Map<String, List<DependencyValidator>> validatorMap) {
    Map<String, Set<String>> vGraph = new LinkedHashMap<>();

    for (Map.Entry<String, List<DependencyValidator>> entry : validatorMap.entrySet()) {
      String key = entry.getKey();
      vGraph.put(key, new LinkedHashSet<String>());

      for (DependencyValidator dataValidator : entry.getValue()) {

        Set<String> dependsOn = dataValidator.getDependsOn();
        if (CollectionUtils.isNotEmpty(dependsOn)) {

          vGraph.get(key).addAll(dependsOn);
        }
      }
    }

    return vGraph;
  }
}
