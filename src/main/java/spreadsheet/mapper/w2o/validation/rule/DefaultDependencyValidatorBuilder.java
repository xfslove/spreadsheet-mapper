package spreadsheet.mapper.w2o.validation.rule;

import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.w2o.validation.validator.cell.DependencyValidator;
import spreadsheet.mapper.w2o.validation.validator.cell.MultiCellValidator;
import spreadsheet.mapper.w2o.validation.validator.cell.SingleCellValidator;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Created by hanwen on 2017/1/20.
 */
public class DefaultDependencyValidatorBuilder implements DependencyValidatorBuilder {

  private List<DependencyValidator> validators = new ArrayList<>();

  @Override
  public DependencyRuleBuilder rule(String name) {
    return new DefaultDependencyRuleBuilder(name);
  }

  @Override
  public List<DependencyValidator> build() {
    return validators;
  }

  public class DefaultDependencyRuleBuilder implements DependencyRuleBuilder {

    private String name;

    private List<String> matchFields = new ArrayList<>();

    private DependencyRuleParam param = new DependencyRuleParam();

    DefaultDependencyRuleBuilder(String name) {
      this.name = name;
    }

    @Override
    public DependencyRuleBuilder matchFields(String... matchFields) {
      if (matchFields == null) {
        return this;
      }
      Collections.addAll(this.matchFields, matchFields);
      return this;
    }

    @Override
    public DependencyRuleBuilder group(String group) {
      param.setGroup(group);
      return this;
    }

    @Override
    public DependencyRuleBuilder dependsOn(String... dependsOn) {
      if (dependsOn == null) {
        return this;
      }
      param.setDependsOn(Arrays.asList(dependsOn));
      return this;
    }

    @Override
    public DependencyRuleBuilder errorMessage(String errorMessage) {
      param.setErrorMessage(errorMessage);
      return this;
    }

    @Override
    public DependencyRuleBuilder param(Object additionalParam) {
      param.setAdditionalParam(additionalParam);
      return this;
    }

    @Override
    public DependencyValidatorBuilder end() {

      DependencyValidatorFactory factory = DependencyValidatorFactoryRegisterer.GLOBAL.getFactoryInstance(name);

      if (factory instanceof MultiCellValidatorFactory) {

        MultiCellValidatorFactory multiCellValidatorFactory = (MultiCellValidatorFactory) factory;
        MultiCellValidator validator = multiCellValidatorFactory.create(param, matchFields);
        DefaultDependencyValidatorBuilder.this.validators.add(validator);

      } else if (factory instanceof SingleCellValidatorFactory) {

        SingleCellValidatorFactory singleCellValidatorFactory = (SingleCellValidatorFactory) factory;

        boolean groupNull = StringUtils.isBlank(param.getGroup());

        for (String field : matchFields) {

          if (groupNull) {
            param.setGroup(field);
          }

          SingleCellValidator validator = singleCellValidatorFactory.create(param, field);
          DefaultDependencyValidatorBuilder.this.validators.add(validator);
        }

      }

      return DefaultDependencyValidatorBuilder.this;
    }

  }

}
