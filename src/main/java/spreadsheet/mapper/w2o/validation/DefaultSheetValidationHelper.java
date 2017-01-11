package spreadsheet.mapper.w2o.validation;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.model.msg.ErrorMessage;
import spreadsheet.mapper.model.msg.ErrorMessageBean;
import spreadsheet.mapper.model.msg.MessageWriteStrategies;
import spreadsheet.mapper.w2o.process.WorkbookProcessException;
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

  private Sheet sheet;

  private SheetMeta sheetMeta;

  /*===============
    validators
   ================*/
  private List<SheetValidator> sheetValidators = new ArrayList<>();
  private List<RowValidator> rowValidators = new ArrayList<>();
  private List<CellValidator> cellValidators = new ArrayList<>();

  /*==============
    error messages
   ===============*/
  private List<ErrorMessage> errorMessages = new ArrayList<>();

  /*============
    valid result
   =============*/
  private boolean validResult = true;

  @Override
  public SheetValidationHelper sheetValidator(SheetValidator... validators) {
    if (validators == null) {
      return this;
    }
    Collections.addAll(this.sheetValidators, validators);
    return this;
  }

  @Override
  public SheetValidationHelper rowValidator(RowValidator... validators) {
    if (validators == null) {
      return this;
    }
    Collections.addAll(this.rowValidators, validators);
    return this;
  }

  @Override
  public SheetValidationHelper cellValidator(CellValidator... validators) {
    if (validators == null) {
      return this;
    }
    Collections.addAll(this.cellValidators, validators);
    return this;
  }

  @Override
  public SheetValidationHelper sheet(Sheet sheet) {
    this.sheet = sheet;
    return this;
  }

  @Override
  public SheetValidationHelper sheetMeta(SheetMeta sheetMeta) {
    this.sheetMeta = sheetMeta;
    return this;
  }

  public List<ErrorMessage> getErrorMessages() {
    return errorMessages;
  }

  @Override
  public boolean valid() {
    if (sheet == null) {
      throw new WorkbookValidateException("set sheet first");
    }

    if (sheetMeta == null) {
      throw new WorkbookProcessException("set sheet meta first");
    }

    // check dependency of this sheet
    checkValidatorKeyDependency();

    validSheet(sheet);

    if (!validResult) {
      return false;
    }

    for (int i = sheetMeta.getDataStartRowIndex(); i <= sheet.sizeOfRows(); i++) {
      validRowCells(sheet.getRow(i));
    }

    return validResult;
  }

  /**
   * check if dependency correct
   */
  private void checkValidatorKeyDependency() {
    Map<String, List<DependencyValidator>> validatorMap = buildRelationValidatorMap();

    Map<String, Set<String>> vGraph = DependencyEngineHelper.buildVGraph(validatorMap);
    Set<String> allGroups = vGraph.keySet();
    for (Set<String> dependsOn : vGraph.values()) {
      if (!CollectionUtils.subtract(dependsOn, allGroups).isEmpty()) {
        throw new WorkbookValidateException("depends on missing group.");
      }
    }

    DependencyCycleCheckEngine dependencyCycleCheckEngine = new DependencyCycleCheckEngine(validatorMap);
    if (dependencyCycleCheckEngine.cycling()) {
      throw new WorkbookValidateException("dependency cycling.");
    }
  }

  /*=========================
   below is internal valid
   ==========================*/
  private void validSheet(Sheet sheet) {
    for (SheetValidator validator : sheetValidators) {

      if (!validator.valid(sheet, sheetMeta)) {
        validResult = false;

        String errorMessage = validator.getErrorMessage();

        if (StringUtils.isNotBlank(errorMessage)) {

          this.errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.TEXT_BOX, errorMessage, sheet.getIndex()));
        }
      }
    }
  }

  private void validRowCells(Row row) {
    Map<String, List<DependencyValidator>> validatorMap = buildRelationValidatorMap();

    DependencyValidateEngine dependencyValidateEngine = new DependencyValidateEngine(validatorMap, sheetMeta, row);

    validResult = dependencyValidateEngine.valid();

    this.errorMessages.addAll(dependencyValidateEngine.getErrorMessages());
  }

  private Map<String, List<DependencyValidator>> buildRelationValidatorMap() {
    // one key corresponding multi validators, row validators first
    Map<String, List<DependencyValidator>> validatorMap = new LinkedHashMap<>();

    for (RowValidator validator : rowValidators) {
      String group = validator.getGroup();
      if (!validatorMap.containsKey(group)) {
        validatorMap.put(group, new ArrayList<DependencyValidator>());
      }
      validatorMap.get(group).add(validator);
    }

    for (CellValidator validator : cellValidators) {
      String group = validator.getGroup();
      if (!validatorMap.containsKey(group)) {
        validatorMap.put(group, new ArrayList<DependencyValidator>());
      }
      validatorMap.get(group).add(validator);
    }

    return validatorMap;
  }
}
