package spreadsheet.mapper.w2o.validation.rule;

import spreadsheet.mapper.w2o.validation.validator.cell.DependencyValidator;
import spreadsheet.mapper.w2o.validation.validator.cell.SingleCellValidator;

import java.util.List;

/**
 * dependency validator builder
 * <p>
 * Created by hanwen on 2017/1/23.
 */
public interface DependencyValidatorBuilder {

  /**
   * start build a validate rule
   *
   * @param name rule name
   * @return {@link DependencyRuleBuilder}
   * @see DependencyValidatorFactoryRegisterer
   */
  DependencyRuleBuilder rule(String name);

  /**
   * build validators from supplied rules
   *
   * @return {@link SingleCellValidator}
   */
  List<DependencyValidator> build();

  /**
   * dependency validate rule builder
   */
  interface DependencyRuleBuilder {

    /**
     * @param matchFields {@link SingleCellValidator#getMatchField()}
     * @return {@link DependencyRuleBuilder}
     */
    DependencyRuleBuilder matchFields(String... matchFields);

    /**
     * if empty default is field
     *
     * @param group {@link SingleCellValidator#getGroup()}
     * @return {@link DependencyRuleBuilder}
     */
    DependencyRuleBuilder group(String group);

    /**
     * @param dependsOn {@link SingleCellValidator#getDependsOn()}
     * @return {@link DependencyRuleBuilder}
     */
    DependencyRuleBuilder dependsOn(String... dependsOn);

    /**
     * @param errorMessage {@link SingleCellValidator#getErrorMessage()}
     * @return {@link DependencyRuleBuilder}
     */
    DependencyRuleBuilder errorMessage(String errorMessage);

    /**
     * @param additionalParam the additional param validator need
     * @return {@link DependencyRuleBuilder}
     */
    DependencyRuleBuilder param(Object additionalParam);

    /**
     * finish a rule
     *
     * @return {@link DependencyValidatorBuilder}
     */
    DependencyValidatorBuilder end();
  }
}
