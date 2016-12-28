package me.excel.tools.processor;

import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * customer data processor
 * <p>
 * Created by hanwen on 16-1-18.
 */
public class CommonProcessorListener<D> implements ObjectProcessorListener<D> {

  private Consumer<D> handleAcceptor;

  public CommonProcessorListener(Consumer<D> handleAcceptor) {
    this.handleAcceptor = handleAcceptor;
  }

  @Override
  public void beforeRow(D model) {
    // nothing
  }

  @Override
  public void afterRow(D model) {
    // nothing
  }

  @Override
  public void afterSheet(List<D> models) {
    Optional<Consumer<D>> handleAcceptor = Optional.ofNullable(this.handleAcceptor);
    handleAcceptor.ifPresent(models::forEach);
  }
}
