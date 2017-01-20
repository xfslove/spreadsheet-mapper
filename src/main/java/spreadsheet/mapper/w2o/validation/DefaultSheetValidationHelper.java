package spreadsheet.mapper.w2o.validation;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.model.msg.Message;
import spreadsheet.mapper.model.msg.MessageBean;
import spreadsheet.mapper.model.msg.MessageWriteStrategies;
import spreadsheet.mapper.w2o.validation.engine.DependencyCycleCheckEngine;
import spreadsheet.mapper.w2o.validation.engine.DependencyEngineHelper;
import spreadsheet.mapper.w2o.validation.engine.DependencyValidateEngine;
import spreadsheet.mapper.w2o.validation.validator.DependencyValidator;
import spreadsheet.mapper.w2o.validation.validator.cell.CellValidator;
import spreadsheet.mapper.w2o.validation.validator.row.RowValidator;
import spreadsheet.mapper.w2o.validation.validator.sheet.SheetValidator;

import java.util.*;

/**
 * Created by hanwen on 15-12-16.
 */
public class DefaultSheetValidationHelper implements SheetValidationHelper {

  /*===============
    validators
   ================*/
  private List<SheetValidator> sheetValidators = new ArrayList<>();
  private List<RowValidator> rowValidators = new ArrayList<>();
  private List<CellValidator> cellValidators = new ArrayList<>();

  private List<Message> errorMessages = new ArrayList<>();
  private boolean validResult = true;

  @Override
  public SheetValidationHelper addSheetValidator(SheetValidator sheetValidator) {
    if (sheetValidator == null) {
      throw new IllegalArgumentException("sheet validator can not be null");
    }
    sheetValidators.add(sheetValidator);
    return this;
  }

  @Override
  public SheetValidationHelper addRowValidator(RowValidator rowValidator) {
    if (rowValidator == null) {
      throw new IllegalArgumentException("row validator can not be null");
    }
    rowValidators.add(rowValidator);
    return this;
  }

  @Override
  public SheetValidationHelper addCellValidator(CellValidator cellValidator) {
    if (cellValidator == null) {
      throw new IllegalArgumentException("cell validator can not be null");
    }
    cellValidators.add(cellValidator);
    return this;
  }

  @Override
  public List<Message> getErrorMessages() {
    return errorMessages;
  }

  @Override
  public boolean valid(Sheet sheet, SheetMeta sheetMeta) {
    // check dependency of this sheet
    checkValidatorGroupDependency();

    validSheet(sheet, sheetMeta);

    if (!validResult) {
      return false;
    }

    for (int i = sheetMeta.getDataStartRowIndex(); i <= sheet.sizeOfRows(); i++) {
      validRowCells(sheet.getRow(i), sheetMeta);
    }

    return validResult;
  }

  /**
   * check if dependency correct
   */
  private void checkValidatorGroupDependency() {
    Map<String, List<DependencyValidator>> validatorMap = buildRelationValidatorMap();

    Map<String, Set<String>> vGraph = DependencyEngineHelper.buildVGraph(validatorMap);
    Set<String> allGroups = vGraph.keySet();

    for (Map.Entry<String, Set<String>> entry : vGraph.entrySet()) {
      String group = entry.getKey();
      Set<String> dependsOn = entry.getValue();

      Collection missingGroups = CollectionUtils.subtract(dependsOn, allGroups);
      if (!missingGroups.isEmpty()) {
        throw new WorkbookValidateException("[" + group + "]depends on missing group:" + missingGroups.toString());
      }
    }

    DependencyCycleCheckEngine dependencyCycleCheckEngine = new DependencyCycleCheckEngine(vGraph);
    if (dependencyCycleCheckEngine.cycling()) {
      throw new WorkbookValidateException("dependency exists cycle:" + dependencyCycleCheckEngine.getCycle().toString());
    }
  }

  /*=========================
   below is internal valid
   ==========================*/
  private void validSheet(Sheet sheet, SheetMeta sheetMeta) {
    for (SheetValidator validator : sheetValidators) {

      if (!validator.valid(sheet, sheetMeta)) {
        validResult = false;

        String errorMessage = validator.getErrorMessage();

        if (StringUtils.isNotBlank(errorMessage)) {

          this.errorMessages.add(new MessageBean(MessageWriteStrategies.TEXT_BOX, errorMessage, sheet.getIndex()));
        }
      }
    }
  }

  private void validRowCells(Row row, SheetMeta sheetMeta) {
    Map<String, List<DependencyValidator>> validatorMap = buildRelationValidatorMap();

    DependencyValidateEngine dependencyValidateEngine = new DependencyValidateEngine(validatorMap);

    validResult = dependencyValidateEngine.valid(row, sheetMeta);

    this.errorMessages.addAll(dependencyValidateEngine.getErrorMessages());
  }

  private Map<String, List<DependencyValidator>> buildRelationValidatorMap() {
    // one key corresponding multi validators, row validators first
    Map<String, List<DependencyValidator>> validatorMap = new LinkedHashMap<>();

    for (RowValidator validator : rowValidators) {
      String group = validator.getGroup();
      if (StringUtils.isBlank(group)) {
        throw new WorkbookValidateException("row validator[" + validator.getClass().getName() + "] group can not be null");
      }

      if (!validatorMap.containsKey(group)) {
        validatorMap.put(group, new ArrayList<DependencyValidator>());
      }
      validatorMap.get(group).add(validator);
    }

    for (CellValidator validator : cellValidators) {
      String group = validator.getGroup();
      if (StringUtils.isBlank(group)) {
        throw new WorkbookValidateException("cell validator[" + validator.getClass().getName() + "] group can not be null");
      }

      if (!validatorMap.containsKey(group)) {
        validatorMap.put(group, new ArrayList<DependencyValidator>());
      }
      validatorMap.get(group).add(validator);
    }

    return validatorMap;
  }
}
