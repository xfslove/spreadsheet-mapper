package extensible.sheet.w2o.validator;

import java.util.Set;

/**
 * relation validator, after workbook and sheet validators, if post validators failure, relation validators will skip.
 * <p>
 * Created by hanwen on 2016/12/26.
 */
public interface RelationValidator extends Validator {

  /**
   * the validator group name
   *
   * @return the group name
   */
  String getGroup();

  /**
   * <pre>
   * the validator do valid after depends on group validators.
   * the group validators sequence is the sequence of add to validate engine.
   * notice:
   * 1. if in group one valid failure the rest validators will skip.
   * 2. all depends on group valid passed will do this validator.
   * </pre>
   *
   * @return depends on groups
   */
  Set<String> getDependsOn();
}
