package me.excel.tools.validator.data;

import java.util.Set;

/**
 * data validator, after template validators, if template validators failure, data validator will skip.
 * <p>
 * Created by hanwen on 2016/12/26.
 */
public interface DataValidator {

  /**
   * the validator key to identify unique validator
   *
   * @return the key
   */
  String getKey();

  /**
   * the validator depends on which validators result, means if any one depends on validator failure, this validator will skip.
   *
   * @return depends on keys
   */
  Set<String> getDependsOn();
}
