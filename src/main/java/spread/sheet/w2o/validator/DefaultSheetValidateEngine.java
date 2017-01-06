package spread.sheet.w2o.validator;

import org.apache.commons.collections.CollectionUtils;
import spread.sheet.model.core.Cell;
import spread.sheet.model.core.Row;
import spread.sheet.model.core.Sheet;
import spread.sheet.model.message.ErrorMessage;
import spread.sheet.model.message.ErrorMessageBean;
import spread.sheet.model.message.MessageWriteStrategies;
import spread.sheet.model.meta.FieldMeta;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.w2o.processor.WorkbookProcessException;
import spread.sheet.w2o.validator.cell.CellValidator;
import spread.sheet.w2o.validator.row.RowValidator;
import spread.sheet.w2o.validator.sheet.SheetValidator;

import java.util.*;

/**
 * Created by hanwen on 15-12-16.
 */
public class DefaultSheetValidateEngine implements SheetValidateEngine {

  private Sheet sheet;

  private SheetMeta sheetMeta;

  /*===============
    validators
   ================*/
  private List<SheetValidator> sheetValidators = new ArrayList<>();
  private Map<String, List<RowValidator>> group2rowValidators = new HashMap<>();
  private Map<String, List<CellValidator>> group2cellValidators = new HashMap<>();

  /*==============
    error messages
   ===============*/
  private List<ErrorMessage> errorMessages = new ArrayList<>();

  @Override
  public SheetValidateEngine sheetValidator(SheetValidator... validators) {
    if (validators == null) {
      return this;
    }
    Collections.addAll(this.sheetValidators, validators);
    return this;
  }

  @Override
  public SheetValidateEngine rowValidator(RowValidator... validators) {
    if (validators == null) {
      return this;
    }

    for (RowValidator validator : validators) {
      String group = validator.getGroup();

      if (!group2rowValidators.containsKey(group)) {
        group2rowValidators.put(group, new ArrayList<RowValidator>());
      }
      group2rowValidators.get(group).add(validator);
    }
    return this;
  }

  @Override
  public SheetValidateEngine cellValidator(CellValidator... validators) {
    if (validators == null) {
      return this;
    }
    for (CellValidator validator : validators) {
      String group = validator.getGroup();

      if (!group2cellValidators.containsKey(group)) {
        group2cellValidators.put(group, new ArrayList<CellValidator>());
      }
      group2cellValidators.get(group).add(validator);
    }
    return this;
  }

  @Override
  public SheetValidateEngine sheet(Sheet sheet) {
    this.sheet = sheet;
    return this;
  }

  @Override
  public SheetValidateEngine sheetMeta(SheetMeta sheetMeta) {
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

    if (CollectionUtils.isNotEmpty(errorMessages)) {
      return false;
    }

    for (Row row : sheet.getRows()) {

      validRowCells(row);
    }

    return CollectionUtils.isEmpty(errorMessages);
  }

  /**
   * check if dependency correct
   */
  private void checkValidatorKeyDependency() {
    Map<String, List<RelationValidator>> validatorMap = buildRelationValidatorMap();
    Map<String, Set<String>> dependsOnHierarchy = buildDependsOnHierarchy(validatorMap);

    Set<String> allGroups = dependsOnHierarchy.keySet();
    for (Set<String> dependsOns : dependsOnHierarchy.values()) {
      if (!CollectionUtils.subtract(dependsOns, allGroups).isEmpty()) {
        throw new WorkbookValidateException("depends on missing group.");
      }
    }

    CyclingChecker cyclingChecker = new CyclingChecker(dependsOnHierarchy);
    if (cyclingChecker.cycling()) {
      throw new WorkbookValidateException("dependency cycling.");
    }
  }

  /*=========================
   below is internal valid
   ==========================*/
  private void validSheet(Sheet sheet) {

    for (SheetValidator validator : sheetValidators) {

      if (!validator.valid(sheet, sheetMeta)) {
        String errorMessage = validator.getErrorMessage();
        this.errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.TEXT_BOX, errorMessage, sheet.getIndex()));
      }
    }

  }

  private void validRowCells(Row row) {
    Map<String, List<RelationValidator>> validatorMap = buildRelationValidatorMap();
    Map<String, Set<String>> dependsOnHierarchy = buildDependsOnHierarchy(validatorMap);

    Map<String, Set<Boolean>> allResult = new HashMap<>();

    for (List<RowValidator> validators : group2rowValidators.values()) {
      for (RowValidator validator : validators) {
        allResult.putAll(validRowCellsHierarchy(validatorMap, allResult, dependsOnHierarchy, row, validator.getGroup()));
      }
    }
    for (List<CellValidator> validators : group2cellValidators.values()) {
      for (CellValidator validator : validators) {
        allResult.putAll(validRowCellsHierarchy(validatorMap, allResult, dependsOnHierarchy, row, validator.getGroup()));
      }
    }

  }

  private Map<String, Set<Boolean>> validRowCellsHierarchy(
      Map<String, List<RelationValidator>> validatorMap,
      Map<String, Set<Boolean>> allResult,
      Map<String, Set<String>> dependsOnHierarchy,
      Row row,
      String group
  ) {

    Map<String, Set<Boolean>> dependsOnGroupResult = new HashMap<>();

    if (allResult.containsKey(group)) {
      dependsOnGroupResult.put(group, allResult.get(group));
      return dependsOnGroupResult;
    }

    Set<String> dependsOns = dependsOnHierarchy.get(group);

    if (CollectionUtils.isNotEmpty(dependsOns)) {

      for (String dependsOn : dependsOns) {

        dependsOnGroupResult.putAll(validRowCellsHierarchy(validatorMap, dependsOnGroupResult, dependsOnHierarchy, row, dependsOn));
      }

    }

    if (ifSkip(dependsOnGroupResult)) {
      dependsOnGroupResult.put(group, Collections.singleton((Boolean) null));
      return dependsOnGroupResult;
    }

    Set<Boolean> vrs = new HashSet<>();
    for (RelationValidator relationValidator : validatorMap.get(group)) {
      boolean result = doRowCellsValid(relationValidator, row);
      vrs.add(result);
      // if one of the group valid failure skip rest validator
      if (!result) {
        break;
      }
    }
    dependsOnGroupResult.put(group, vrs);
    return dependsOnGroupResult;
  }

  /**
   * if result is not empty and result set not only has true skip the valid
   *
   * @param result dependsOn result
   * @return true if skip
   */
  private boolean ifSkip(Map<String, Set<Boolean>> result) {
    Set<Boolean> dependsOnVrs = new HashSet<>();
    for (Set<Boolean> vr : result.values()) {
      dependsOnVrs.addAll(vr);
    }

    return !dependsOnVrs.isEmpty() && (dependsOnVrs.size() > 1 || !dependsOnVrs.iterator().next());
  }

  private boolean doRowCellsValid(RelationValidator relationValidator, Row row) {
    if (relationValidator instanceof RowValidator) {

      RowValidator rowValidator = (RowValidator) relationValidator;
      boolean result = rowValidator.valid(row, sheetMeta);

      if (!result) {
        for (String messageOnField : rowValidator.getMessageOnFields()) {
          FieldMeta fieldMeta = sheetMeta.getFieldMeta(messageOnField);

          errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.COMMENT, rowValidator.getErrorMessage(), row.getSheet().getIndex(), row.getIndex(), fieldMeta.getColumnIndex()));
        }
      }

      return result;
    } else {

      CellValidator cellValidator = (CellValidator) relationValidator;
      FieldMeta fieldMeta = sheetMeta.getFieldMeta(cellValidator.getMatchField());

      Cell cell = row.getCell(fieldMeta.getColumnIndex());
      boolean result = cellValidator.valid(cell, fieldMeta);

      if (!result) {
        String messageOnField = cellValidator.getMessageOnField();
        FieldMeta messageOnFieldMeta = sheetMeta.getFieldMeta(messageOnField);
        Cell messageOnCell = row.getCell(messageOnFieldMeta.getColumnIndex());

        errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.COMMENT, cellValidator.getErrorMessage(), row.getSheet().getIndex(), row.getIndex(), messageOnCell.getIndex()));
      }

      return result;
    }
  }

  private Map<String, List<RelationValidator>> buildRelationValidatorMap() {
    // one key corresponding multi validators
    Map<String, List<RelationValidator>> validatorMap = new HashMap<>();

    for (Map.Entry<String, List<RowValidator>> entry : group2rowValidators.entrySet()) {
      String group = entry.getKey();
      if (!validatorMap.containsKey(group)) {
        validatorMap.put(group, new ArrayList<RelationValidator>());
      }
      validatorMap.get(group).addAll(entry.getValue());
    }

    for (Map.Entry<String, List<CellValidator>> entry : group2cellValidators.entrySet()) {
      String group = entry.getKey();
      if (!validatorMap.containsKey(group)) {
        validatorMap.put(group, new ArrayList<RelationValidator>());
      }
      validatorMap.get(group).addAll(entry.getValue());
    }

    return validatorMap;
  }

  private Map<String, Set<String>> buildDependsOnHierarchy(Map<String, List<RelationValidator>> validatorMap) {
    Map<String, Set<String>> dependsOnHierarchy = new HashMap<>();

    for (Map.Entry<String, List<RelationValidator>> entry : validatorMap.entrySet()) {
      String key = entry.getKey();
      dependsOnHierarchy.put(key, new HashSet<String>());

      for (RelationValidator dataValidator : entry.getValue()) {

        Set<String> dependsOn = dataValidator.getDependsOn();
        if (CollectionUtils.isNotEmpty(dependsOn)) {

          dependsOnHierarchy.get(key).addAll(dependsOn);
        }
      }
    }

    return dependsOnHierarchy;
  }
}
