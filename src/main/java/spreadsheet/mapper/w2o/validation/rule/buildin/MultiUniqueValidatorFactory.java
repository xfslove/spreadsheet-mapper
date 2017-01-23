package spreadsheet.mapper.w2o.validation.rule.buildin;

import spreadsheet.mapper.w2o.validation.rule.DependencyRuleParam;
import spreadsheet.mapper.w2o.validation.rule.MultiCellValidatorFactory;
import spreadsheet.mapper.w2o.validation.validator.cell.buildin.MultiUniqueValidator;

import java.util.List;

/**
 * Created by hanwen on 2017/1/22.
 */
public class MultiUniqueValidatorFactory implements MultiCellValidatorFactory<MultiUniqueValidator> {

  @Override
  public MultiUniqueValidator create(DependencyRuleParam param, List<String> matchFields) {
    return new MultiUniqueValidator()
        .group(param.getGroup())
        .dependsOn(param.getDependsOn().toArray(new String[0]))
        .errorMessage(param.getErrorMessage())
        .matchFields(matchFields.toArray(new String[0]));
  }
}
